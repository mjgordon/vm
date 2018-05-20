#include <SDL2/SDL.h>
#include "visualizer.h"


SDLContext context;

uint32_t colors[] = {0xFF000000,
		     0xFF000080,
		     0xFF008000,
		     0xFF008080,
		     0xFF800000,
		     0xFF800080,
		     0xFF808000,
		     0xFFAAAAAA,
		     0xFF555555,
		     0xFF0000FF,
		     0xFF00FF00,
		     0xFF00FFFF,
		     0xFFFF0000,
		     0xFFFF00FF,
		     0xFFFFFF00,
		     0xFFFFFFFF};

uint8_t pixels[1024 * 1024 * 3];

void setupSDL() {
  for (int i =0; i < (1024 * 1024 * 3); i++) {
    pixels[i] = 0;
  }
  
  if (SDL_Init(SDL_INIT_VIDEO) != 0) {
    printf("nope\n");
    return;
  }
  
  context.win = SDL_CreateWindow("Hello World",100,100,1000,1000,SDL_WINDOW_SHOWN);
  if (!context.win){
    printf("windowNope\n");
    SDL_Quit();
    return;
  }

  context.ren = SDL_CreateRenderer(context.win,-1,SDL_RENDERER_ACCELERATED | SDL_RENDERER_PRESENTVSYNC);
  if (!context.ren) {
    SDL_DestroyWindow(context.win);
    printf("renderNope\n");
    SDL_Quit();
    return;
  }

/*   Uint32 rmask, gmask, bmask, amask; */

/* #if SDL_BYTEORDER == SDL_BIG_ENDIAN */
/*   rmask = 0xff000000; */
/*   gmask = 0x00ff0000; */
/*   bmask = 0x0000ff00; */
/*   amask = 0x000000ff; */
/* #else */
/*   rmask = 0x000000ff; */
/*   gmask = 0x0000ff00; */
/*   bmask = 0x00ff0000; */
/*   amask = 0xff000000; */
/* #endif */

/*   context.surface = SDL_CreateRGBSurface(0,1024,1024,32,rmask,gmask,bmask,amask); */
/*   if (context.surface == NULL) { */
/*     SDL_Log("SDL_CreateRGBSurface() failed: %s", SDL_GetError()); */
/*     exit(1); */
/*   } */



  

  context.tex = SDL_CreateTexture(context.ren,SDL_PIXELFORMAT_RGB888,SDL_TEXTUREACCESS_STREAMING,1024,1024);

  if (!context.tex) {
    SDL_DestroyRenderer(context.ren);
    SDL_DestroyWindow(context.win);
    printf("textureNope");
    SDL_Quit();
    return;
  }

  SDL_RenderClear(context.ren);
  SDL_RenderCopy(context.ren,context.tex,NULL,NULL);
  SDL_RenderPresent(context.ren);
}



void cleanupSDL() {
  SDL_DestroyTexture(context.tex);
  SDL_DestroyRenderer(context.ren);
  SDL_DestroyWindow(context.win);
  SDL_Quit();
}

// SDL_Surface pixel setting functions based on code by Benjamin Lindley
// Originally take from https://stackoverflow.com/questions/6852055/how-can-i-modify-pixels-using-sdl

void putPixel32_nolock(SDL_Surface * surface, int x, int y, uint32_t color) {
    uint8_t * pixel = (uint8_t*)surface->pixels;
    pixel += (y * surface->pitch) + (x * sizeof(uint32_t));
    *((uint32_t*)pixel) = color;
}

void putPixel32(SDL_Surface * surface, int x, int y, uint32_t color) {
    if( SDL_MUSTLOCK(surface) )
        SDL_LockSurface(surface);
    putPixel32_nolock(surface, x, y, color);
    if( SDL_MUSTLOCK(surface) )
        SDL_UnlockSurface(surface);
}
