
#include "NpEngine.h"
#include "pch.h"
#include <crtdbg.h>
#include <fstream>
#include <cmath>
#include <mkl.h>
// #include "mat.pb.h"

namespace NNC
{
    namespace NN {
        #define ALIGN64 __declspec(align(64))
        #define ALIGN_AVX   ALIGN64
        const auto NN_SIZE = 256;
        constexpr auto SZ_AVX = 32;
        constexpr auto SZ_CACHE = 64;
        typedef float weight_t;
        /// <summary>
        /// Pointer to a multiple of WeightsPerRegister weight_t items suitably 
        /// aligned for transfer to AVX registers -- an integral multiple 
        /// of 8 floats aligned on 32 byte boundary.
        /// </summary>
        typedef ALIGN_AVX float* Weights;
        /// <summary>
        /// Number of weight_t items per __m256 register (== 8)
        /// </summary>
        constexpr auto WeightsPerRegister = SZ_AVX / sizeof(weight_t);

        typedef __declspec(align(64)) float Block[8][8];
        typedef float ACC[2][NN_SIZE];
        // ----------------------------------------------------------------
        // MUL  -- vectorized multiplication        -- MUL(X,Y)     = X*Y
        #define MUL _mm256_mul_ps
        // FMAC -- vectorized "fused multiply add"  -- FMAC(X,Y,Z)  = X*Y+Z
        #define FMAC _mm256_fmadd_ps
        // H_ADD -- Horizontal add:
        // H_ADD(X,Y) = { X[0]+X[1], X[2]+X[3], Y[0]+Y[1], Y[2]+Y[3],
        //                X[4]+X[5], X[6]+X[7], Y[4]+Y[5], Y[6]+Y[7] }
        #define H_ADD _mm256_hadd_ps
        // LD -- load vector register from floating point address
        #define LD(w) _mm256_load_ps((float*)&w)
        // ----------------------------------------------------------------
        /// <summary>
        /// Allocate n*sz bytes of cache aligned memory
        /// </summary>
        /// <param name="n"></param>
        /// <param name="sz"></param>
        /// <returns></returns>
        void* alloc64(size_t n, size_t sz)
        {
            if (void* mem = _aligned_malloc(n * sz, SZ_CACHE))
            {
                Assert(0 == (uintptr_t(mem) & (SZ_CACHE - 1)));
                return mem;
            }
            std::cerr << "alloc64: Failed to allocate " << n * sz
                << "bytes with cache alignment." << std::endl;
            exit(EXIT_FAILURE);
        }
        /// <summary>
        /// Reorder weights into a (row major) array of 8x8 BLOCKs.
        /// In this order blocked matrix-vector multiplication (sgemv8x8)
        /// accesses weight array memory sequentially. 
        /// With this memory layout sgemv8x8 runs 15% faster than with
        /// the weight array in the usual row/col arrangement.
        /// </summary>
        /// <param name="res">float weights[m_outputs][n_inputs]</param>
        /// <param name="weights">float weights[m_outputs][n_inputs]</param>
        /// <param name="m_outputs"></param>
        /// <param name="n_inputs"></param>
        /// <returns>res</returns>
        Weights block8x8(Block& res, float* weights, int m_outputs, int n_inputs)
        {
            Assert(m_outputs % 8 == 0 && n_inputs % 8 == 0);
            const size_t m = m_outputs;
            const size_t n = n_inputs / WeightsPerRegister;

#define W(i, j) _mm256_load_ps(&weights[8*(n * (i) + (j))])
            __m256* w = (__m256*)res;
            for (int i = 0; i < m; i += 8)
                for (int j = 0; j < n; j++) {
                    *w++ = W(i + 0, j);
                    *w++ = W(i + 1, j);
                    *w++ = W(i + 2, j);
                    *w++ = W(i + 3, j);
                    *w++ = W(i + 4, j);
                    *w++ = W(i + 5, j);
                    *w++ = W(i + 6, j);
                    *w++ = W(i + 7, j);
                }
            return (Weights)res;
#undef W
        }

        template <const int n>
        void ReLU(float output[n], const float input[n])
        {
            if (n % 32 == 0) {
                const __m256 zero = _mm256_setzero_ps();
                __m256* res = (__m256*)output;
                __m256* in = (__m256*)input;
                for (int i = 0; i < n / 8; i += 4) {
                    res[i + 0] = _mm256_max_ps(zero, in[i + 0]);
                    res[i + 1] = _mm256_max_ps(zero, in[i + 1]);
                    res[i + 2] = _mm256_max_ps(zero, in[i + 2]);
                    res[i + 3] = _mm256_max_ps(zero, in[i + 3]);
                }
            }
            else if (n % 8 == 0) {
                const __m256 zero = _mm256_setzero_ps();
                __m256* res = (__m256*)output;
                __m256* in = (__m256*)input;
                for (int i = 0; i < n / 8; ++i) {
                    res[i] = _mm256_max_ps(zero, in[i]);
                }
            }
            else {
                for (int i = 0; i < n; ++i) {
                    output[i] = fmax(0.0f, input[i]);
                }
            }
        }

        // ----------------------------------------------------------------
        /// <summary>
        /// compute output of N x M fully connected layer with ReLU activation
        /// Y = ReLU(W*X+B)
        /// Blocked Matrix-Vector product using 8x8 blocks
        ///
        /// float W[m_out][n_in], X[n_in], B[m_out], output[m_out]
        /// The parameters are passed as arrays of floats
        /// </summary>
        /// <param name="wgt"></param>
        /// <param name="bias"></param>
        /// <param name="inputs"></param>
        /// <param name="res"></param>
        template <const int M, const int N>
        void sgemvBlock(const Block wgt[M / 8][N / 8], const weight_t bias[M], const weight_t inputs[N],
            Weights res, bool relu)
        {
            constexpr int m = M / 8;
            constexpr int n = N / 8;
            const __m256 zero = _mm256_setzero_ps();
            for (int i = 0; i < m; ++i) {
                // initialization to zero and loop over FMAC 3-4% faster than initialization 
                // by MUL followed by FMAC. VS 2019 unrolls loop once (in absence of MUL init) 
                // which is probably why it is faster than explicit MUL initialization.
                __m256 ymm0 = zero;
                __m256 ymm1 = zero;
                __m256 ymm2 = zero;
                __m256 ymm3 = zero;
                __m256 ymm4 = zero;
                __m256 ymm5 = zero;
                __m256 ymm6 = zero;
                __m256 ymm7 = zero;

                for (int j = 0; j < n; ++j) {
                    __m256 X_j = LD(inputs[8 * j]);
                    ymm0 = FMAC(LD(wgt[i][j][0]), X_j, ymm0);
                    ymm1 = FMAC(LD(wgt[i][j][1]), X_j, ymm1);
                    ymm2 = FMAC(LD(wgt[i][j][2]), X_j, ymm2);
                    ymm3 = FMAC(LD(wgt[i][j][3]), X_j, ymm3);
                    ymm4 = FMAC(LD(wgt[i][j][4]), X_j, ymm4);
                    ymm5 = FMAC(LD(wgt[i][j][5]), X_j, ymm5);
                    ymm6 = FMAC(LD(wgt[i][j][6]), X_j, ymm6);
                    ymm7 = FMAC(LD(wgt[i][j][7]), X_j, ymm7);
                }
                // ----------------------------------------------------------------
                // Horizontal reduction -- sum the elements of each vector register
                // and store those sums in the corresponding element of ymm0, ie.
                // ymm0[i] = sum(ymm_i).

                // H_ADD -- Horizontal add:
                // H_ADD(X,Y) = { X[0]+X[1], X[2]+X[3], Y[0]+Y[1], Y[2]+Y[3],
                //                X[4]+X[5], X[6]+X[7], Y[4]+Y[5], Y[6]+Y[7] }
                ymm0 = H_ADD(ymm0, ymm1);
                ymm2 = H_ADD(ymm2, ymm3);
                ymm4 = H_ADD(ymm4, ymm5);
                ymm6 = H_ADD(ymm6, ymm7);

                ymm0 = H_ADD(ymm0, ymm2);
                ymm4 = H_ADD(ymm4, ymm6);

                __m128 quad1 = _mm_add_ps(_mm256_castps256_ps128(ymm0),
                    _mm256_extractf128_ps(ymm0, 1));
                __m128 quad2 = _mm_add_ps(_mm256_castps256_ps128(ymm4),
                    _mm256_extractf128_ps(ymm4, 1));
                ymm0 = _mm256_insertf128_ps(_mm256_castps128_ps256(quad1), quad2, 1);

                // ----------------------------------------------------------------
                // Add bias, apply ReLU activation, and store result
#define Y(i) *(__m256*)&res[8 * i]
                ymm0 = _mm256_add_ps(ymm0, LD(bias[8 * i]));
                if (relu) {
                    ymm0 = _mm256_max_ps(ymm0, zero);
                }
                Y(i) = ymm0;
#undef Y
            }
        }


        template<const int m, const int n>
        void sgemv(weight_t W[m][n], weight_t bias[m], const weight_t input[n],
            Weights res)
        {
#if 0
            // VS 2019 vectorizes and parallelizes this loop, but only two-fold
            // Not quite as good as MKL blas or four-fold parallel below.
            for (int i = 0; i < m; ++i) {
                float dot = bias[i];
                for (int j = 0; j < n; ++j) {
                    dot += W[i][j] * input[j];
                }
                res[i] = dot;
            }
#endif
#if 1
            // 2-3% better than explicit vectorization -- not sure why.
            std::memcpy(res, bias, m * sizeof(weight_t));
            cblas_sgemv(CblasRowMajor, CblasNoTrans, m, n, 1.0f, (const float*)W, n, input, 1, 1.0f, res, 1);
#endif
#if 0
            const __m256 zero = _mm256_setzero_ps();
            for (int i = 0; i < m; ++i) {

                __m256 dot0 = zero;
                __m256 dot1 = zero;
                __m256 dot2 = zero;
                __m256 dot3 = zero;

                for (int j = 0; j < n; j += 32) {
                    dot0 = FMAC(LD(W[i][j + 0]), LD(input[j + 0]), dot0);
                    dot1 = FMAC(LD(W[i][j + 8]), LD(input[j + 8]), dot1);
                    dot2 = FMAC(LD(W[i][j + 16]), LD(input[j + 16]), dot2);
                    dot3 = FMAC(LD(W[i][j + 24]), LD(input[j + 24]), dot3);
                }
                dot0 = _mm256_add_ps(dot0, dot1);
                dot2 = _mm256_add_ps(dot2, dot3);
                dot0 = _mm256_add_ps(dot0, dot2);
                __m128 r4 = _mm_add_ps(_mm256_castps256_ps128(dot0),
                    _mm256_extractf128_ps(dot0, 1));
                __m128 r2 = _mm_add_ps(r4, _mm_movehl_ps(r4, r4));
                __m128 r1 = _mm_add_ss(r2, _mm_movehdup_ps(r2));

                res[i] = bias[i] + _mm_cvtss_f32(r1);
            }
#endif
        }

        /// <summary>
        /// Fully connected NxM layer with optional ReLU activation
        /// M and N must be multiples of 8
        /// </summary>
        template <const int n_inputs, const int m_outputs, const bool relu = false>
        struct fc_layer8x8 {
            typedef const Block W_t[m_outputs / 8][n_inputs / 8];
            typedef const weight_t B_t[m_outputs];
#if 1
            W_t& W;
            B_t& B;
#else
            const Block W[m_outputs / 8][n_inputs / 8];
            const weight_t B[m_outputs];
#endif
            fc_layer8x8(const float* w, const float* b)
                : W(alloc64(n_inputs* m_outputs, sizeof weight_t)), B(alloc64(m_outputs, sizeof weight_t))
            {
                block8x8(W, w, m_outputs, n_inputs);
                std::memcpy(&B, b, m_outputs * sizeof(weight_t));
            }
            void apply(const Weights input, Weights output) {
                sgemvBlock(W, B, input, output, relu);
            }
            weight_t wgt(int i, int j)
            {
                return W[i / 8][j / 8][i % 8][j % 8];
            }
        };

        const weight_t* loadMat(const char* filename, int m, int n)
        {
            using namespace std;
            using namespace mat;
            MatFlt M;
            // Read weight file
            fstream input(filename, ios::in | ios::binary);
            if (!M.ParseFromIstream(&input)) {
                cerr << "Failed to parse weight file." << endl;
                return nullptr;
            }
            Assert(M.has_rows() && M.has_cols());
            Assert(m == M.rows() && n == M.cols());
            return &M.data().Get(0);
        }

        /// <summary>
        /// Fully connected NxM layer with optional ReLU activation
        /// M and N arbitrary.
        /// </summary>
        template <const int n_inputs, const int m_outputs, const bool relu = false>
        struct fc_layer {
            const weight_t W[m_outputs][n_inputs];
            const weight_t B[m_outputs];

            fc_layer(const float* w, const float* b)
            {
                std::memcpy(W, w, n_inputs * m_outputs * sizeof(weight_t));
                std::memcpy(B, b, m_outputs * sizeof(weight_t));
            }
            fc_layer(const char* w, const char* b) : fc_layer(loadMat(w), loadMat(b))
            {}
            void apply(const Weights input, Weights output)
            {
                sgemv(W, B, input, output);
                if (relu)
                    ReLU<m_outputs>(output, output);

            }
            weight_t wgt(int i, int j)
            {
                return W[i][j];
            }
        };
        constexpr auto nKingSq = 64;
        constexpr auto nPieceT = 5;
        constexpr auto nColor = 2;
        constexpr auto nPieceSq = 64;
        constexpr auto E1 = 4;
        constexpr auto E8 = 60;
#if 0
        typedef struct nn_t
        {
            float W0[nKingSq][nColor][nPieceT][nPieceSq][NN_SIZE];
            float B0[NN_SIZE];
            float W1[NN_SIZE * 2 * 32];
            float B1[32];
            float W2[32 * 32];
            float B2[32];
            float W3[32 * 1];
            float B3[1];
        } NN;
#endif
        /// <summary>
        /// Three layer fully connected multi-layer perceptron.
        /// N inputs
        /// Two hidden layers with H1 and H2 neurons
        /// M outputs
        /// </summary>
        template <const int N, const int H1, const int H2, const int M>
        struct mlp_3 {
            typedef float inp[N];
            typedef float h1[H1];
            typedef float h2[H2];
            typedef float outp[M];

            fc_layer8x8<N, H1>  layer1;
            fc_layer8x8<H1, H2> layer2;
            fc_layer<H2, M>     layer3;

            mlp_3(float* w1, float* b1, float* w2, float* b2, float* w3, float* b3)
                : layer1(w1, b1), layer2(w2, b2), layer3(w3, b3)
            {}
            mlp_3(char* w1, char* b1, char* w2, char* b2, char* w3, char* b3)
                : layer1(w1, b1), layer2(w2, b2), layer3(w3, b3)
            {}
            void apply(inp& input, outp& output)
            {
                h1 h1_out;
                h2 h2_out;
                layer1.apply(input, h1);
                layer2.apply(h1, h2);
                layer3.apply(h2, output);
            }
        };




        /// <summary>
        /// incrementally updated input layer
        /// The value Z(x) = W*x+B
        /// </summary>
        struct nnue_layer {

            ALIGN64 float W[nKingSq][nColor][nPieceT][nPieceSq][NN_SIZE];
            ALIGN64 float B[NN_SIZE];
            ALIGN64 ACC   acc;
            int           wksq;
            int           bksq; // rotated black king square

            nnue_layer() { set_kings(E1, E8); }

            void set_kings(int WhiteKingSq, int BlackKingSq)
            {
                wksq = WhiteKingSq;
                bksq = BlackKingSq ^ 63;
            }
            void set_weights(float* w, float* b)
            {
                std::memcpy((void*)W, w, sizeof(W));
                std::memcpy((void*)B, b, sizeof(B));
            }
            void init_layer_input(int WhiteKingSq, int BlackKingSq)
            {
                set_kings(WhiteKingSq, BlackKingSq);
                // Initialize both halves of accumulator to bias values
                std::memcpy((void*)&acc[0], B, sizeof(B));
                std::memcpy((void*)&acc[1], B, sizeof(B));
            }
            void init_layer_input()
            {
                init_layer_input(E1, E8);   // init kings to e1-e8
            }
            void _upd_add_piece_half(float* wgt, float* const acc)
            {
                for (int o = 0; o < NN_SIZE; o++)
                    acc[o] += wgt[o];
            }
            void _upd_del_piece_half(float* wgt, float* const acc)
            {
                for (int o = 0; o < NN_SIZE; o++)
                    acc[o] -= wgt[o];
            }
            void _upd_move_half(int ksq, int color, int piece_type, int from, int to,
                float* const acc)
            {
                const float* const weights_from = &W[ksq][color][piece_type][from][0];
                const float* const weights_to = &W[ksq][color][piece_type][to][0];
                for (int o = 0; o < NN_SIZE; o++)
                    acc[o] += weights_to[o] - weights_from[o];
            }
            void add_piece(int color, int piece_type, int sq)
            {
                _upd_add_piece_half(W[wksq][color][piece_type][sq], acc[0]);
                _upd_add_piece_half(W[bksq][color ^ 1][piece_type][sq ^ 63], acc[1]);
            }
            void del_piece(int color, int piece_type, int sq)
            {
                _upd_del_piece_half(W[wksq][color][piece_type][sq], acc[0]);
                _upd_del_piece_half(W[bksq][color ^ 1][piece_type][sq ^ 63], acc[1]);
            }
            void move_piece(int color, int piece_type, int from, int to)
            {
                _upd_move_half(wksq, color, piece_type, from, to,
                    acc[0]);
                _upd_move_half(bksq, color ^ 1, piece_type, from ^ 63, to ^ 63,
                    acc[1]);
            }
            /// <summary>
            /// Apply activation (ReLU) to accumulated value.
            /// The output order depends on the color of the player to move.
            /// </summary>
            /// <param name="output"></param>
            /// <param name="color"></param>
            void apply(ACC output, int color)
            {
                ReLU<NN_SIZE>(output[color], acc[0]);
                ReLU<NN_SIZE>(output[color ^ 1], acc[1]);
            }
        };

        nnue_layer nnue;
        mlp_3<512, 64, 64, 1> nn();
        void fc_layer_dbg(int m_out, int n_in, Weights weights,
            Weights bias, Weights input, Weights res)
        {

            typedef float W[32][256];
            typedef Block Wb[32 / 8][256 / 8];
            sgemv<32, 256>(reinterpret_cast<W&>(*&weights[0]), bias, input, res);
            //sgemvBlock<32, 256>(reinterpret_cast<Wb&>(*&weights[0]), bias, input, res, false);
#if 0
            if (neq(res[i], tres[i]))
                std::cerr << "sgemv: " << i << " res: " << res[i]
                << " 8x8: " << tres[i] << std::endl;
#endif
        }

    }
}