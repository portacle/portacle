#include<windows.h>

int resolve_path(char *path, char *resolved){
  int len = GetFullPathName(path, MAX_PATH, resolved, 0);
  if(MAX_PATH < len)
    return 1;
  if(len <= 0)
    return 2;
  return 0;
}

int add_font(char *file){
  char font[MAX_PATH] = "";

  if(resolve_path(file, font) != 0)
    return 1;
    
  if(AddFontResource(font) == 0)
    return 2;

  SendMessage(HWND_BROADCAST, WM_FONTCHANGE, 0, 0);
  return 0;
}

int main(int argc, char **argv){
  for(int i=1; i<argc; ++i){
    if(add_font(argv[i]) != 0)
      return i+1;
  }
  return 0;
}
