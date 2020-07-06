// https://github.com/Barnold1953/GraphicsTutorials/blob/master/Bengine/Timing.cpp
// , ported to C

#include "Timing.h"
#include <stdbool.h>

void FpsLimiter_init(FpsLimiter *this, float maxFPS) {
  FpsLimiter_setMaxFPS(this, maxFPS);
}

void FpsLimiter_setMaxFPS(FpsLimiter *this, float maxFPS) {
  this->_maxFPS = maxFPS;
}

void FpsLimiter_begin(FpsLimiter *this) { this->_startTicks = SDL_GetTicks(); }

void FpsLimiter_calculateFPS(FpsLimiter *this);

float FpsLimiter_end(FpsLimiter *this) {
  FpsLimiter_calculateFPS(this);

  float frameTicks = (float)(SDL_GetTicks() - this->_startTicks);
  // Limit the FPS to the max FPS
  if (1000.0f / this->_maxFPS > frameTicks) {
    SDL_Delay((Uint32)(1000.0f / this->_maxFPS - frameTicks));
  }

  return this->_fps;
}

// [Private]
void FpsLimiter_calculateFPS(FpsLimiter *this) {
  // The number of frames to average
  static const int NUM_SAMPLES = 10;
  // Stores all the frametimes for each frame that we will average
  static float frameTimes[NUM_SAMPLES];
  // The current frame we are on
  static int currentFrame = 0;
  // the ticks of the previous frame
  static Uint32 prevTicks;
  static bool didInitPrevTicks = false;
  if (!didInitPrevTicks) {
    prevTicks = SDL_GetTicks();
    didInitPrevTicks = true;
  }

  // Ticks for the current frame
  Uint32 currentTicks = SDL_GetTicks();

  // Calculate the number of ticks (ms) for this frame
  this->_frameTime = (currentTicks - prevTicks);
  frameTimes[currentFrame % NUM_SAMPLES] = (float)this->_frameTime;

  // current ticks is now previous ticks
  prevTicks = currentTicks;

  // The number of frames to average
  int count;

  currentFrame++;
  if (currentFrame < NUM_SAMPLES) {
    count = currentFrame;
  } else {
    count = NUM_SAMPLES;
  }

  // Average all the frame times
  float frameTimeAverage = 0;
  for (int i = 0; i < count; i++) {
    frameTimeAverage += frameTimes[i];
  }
  frameTimeAverage /= count;

  // Calculate FPS
  if (frameTimeAverage > 0) {
    this->_fps = 1000.0f / frameTimeAverage;
  } else {
    this->_fps = 60.0f;
  }
}
