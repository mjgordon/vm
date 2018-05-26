#include <SDL2/SDL.h>
#include "visualizer.h"

SDLContext context;

colorRGB colors[] = {{0xFF000000},
		     {0xFF800000},
		     {0xFF008000},
		     {0xFF808000},
		     {0xFF000080},
		     {0xFF800080},
		     {0xFF008080},
		     {0xFFAAAAAA},
		     {0xFF555555},
		     {0xFFFF0000},
		     {0xFF00FF00},
		     {0xFFFFFF00},
		     {0xFF0000FF},
		     {0xFFFF00FF},
		     {0xFF00FFFF},
		     {0xFFFFFFFF}};

uint8_t pixels[1024 * 1024 * 4];

void setupSDL() {
  
  if (SDL_Init(SDL_INIT_VIDEO) != 0) {
    printf("nope\n");
    return;
  }
  
  context.win = SDL_CreateWindow("VM",100,100,1024,1024,SDL_WINDOW_SHOWN);
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

  context.tex = SDL_CreateTexture(context.ren,SDL_PIXELFORMAT_ARGB8888,SDL_TEXTUREACCESS_STREAMING,1024,1024);
  if (!context.tex) {
    SDL_DestroyRenderer(context.ren);
    SDL_DestroyWindow(context.win);
    printf("textureNope");
    SDL_Quit();
    return;
  }

  updateSDL();
}


void updateSDL() {
  SDL_SetRenderDrawColor(context.ren, 0, 0, 0, 255);
  SDL_RenderClear(context.ren);
  SDL_UpdateTexture(context.tex,NULL,&pixels[0],1024 * 4);
  SDL_RenderCopy(context.ren,context.tex,NULL,NULL);
  SDL_RenderPresent(context.ren);
}

void cleanupSDL() {
  SDL_DestroyTexture(context.tex);
  SDL_DestroyRenderer(context.ren);
  SDL_DestroyWindow(context.win);
  SDL_Quit();
}
