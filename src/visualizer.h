#ifndef VISUALIZER
#define VISUALIZER

#include <SDL2/SDL.h>

typedef struct {
  SDL_Window *win;
  SDL_Renderer *ren;
  SDL_Surface *surface;
  SDL_Texture *tex;
} SDLContext;

typedef union {
  uint32_t integer;
  uint8_t channels[4];
} colorRGB;

extern SDLContext context;
extern colorRGB colors[];
extern uint8_t pixels[];

void setupSDL();
void updateSDL();
void cleanupSDL();

#endif
