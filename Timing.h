#include <SDL2/SDL.h>

typedef struct FpsLimiter {
    // Variables
    float _fps;
    float _maxFPS;
    float _frameTime;
    Uint32 _startTicks;
} FpsLimiter;

void FpsLimiter_init(FpsLimiter* this, float maxFPS);

void FpsLimiter_setMaxFPS(FpsLimiter* this, float maxFPS);

void FpsLimiter_begin(FpsLimiter* this);

float FpsLimiter_end(FpsLimiter* this);