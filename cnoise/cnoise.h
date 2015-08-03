#ifdef CNOISE_EXPORTS
#define CNOISE_API __declspec(dllexport)
#else
#define CNOISE_API __declspec(dllimport)
#endif

CNOISE_API int CN_Simplex3DDeriv(float x, float y, float z, float* results);
