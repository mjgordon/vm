#ifndef VISUALIZER
#define VISUALIZER

#include <SDL2/SDL.h>

typedef struct {
  SDL_Window *win;
  SDL_Renderer *ren;
  SDL_Surface *surface;
  SDL_Texture *tex;
} SDLContext;

extern SDLContext context;
extern uint32_t colors[];
extern uint8_t pixels[];

void setupSDL();
void cleanupSDL();
void randomize();

void putPixel32_nolock(SDL_Surface * surface, int x, int y, uint32_t color);
void putPixel32(SDL_Surface * surface, int x, int y, uint32_t color);

#endif
