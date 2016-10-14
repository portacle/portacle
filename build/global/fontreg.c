#include<windows.h>

int add_font(char *font){
  if(AddFontResource(font) == 0){
    return 0;
  }else{
    SendMessage(HWND_BROADCAST, WM_FONTCHANGE, 0, 0);
    return 1;
  }
}

int main(int argc, char **argv){
  for(int i=0; i<argc; ++i){
    if(add_font(argv[i]) == 0)
      return i+1;
  }
  return 0;
}
