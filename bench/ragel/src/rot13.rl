#include "../common.h"
// Ragel implementation of a rot13 transformation

#define P(C) fputs(C, stdout);

%%{

  machine rot13;

  action print_rest {
    fprintf(stdout, "%.*s", 1, &fc);
  }

  A = 'A' % { P("N") };
  B = 'B' % { P("O") };
  C = 'C' % { P("P") };
  D = 'D' % { P("Q") };
  E = 'E' % { P("R") };
  F = 'F' % { P("S") };
  G = 'G' % { P("T") };
  H = 'H' % { P("U") };
  I = 'I' % { P("V") };
  J = 'J' % { P("W") };
  K = 'K' % { P("X") };
  L = 'L' % { P("Y") };
  M = 'M' % { P("Z") };
  N = 'N' % { P("A") };
  O = 'O' % { P("B") };
  P = 'P' % { P("C") };
  Q = 'Q' % { P("D") };
  R = 'R' % { P("E") };
  S = 'S' % { P("F") };
  T = 'T' % { P("G") };
  U = 'U' % { P("H") };
  V = 'V' % { P("I") };
  W = 'W' % { P("J") };
  X = 'X' % { P("K") };
  Y = 'Y' % { P("L") };
  Z = 'Z' % { P("M") };
  a = 'a' % { P("n") };
  b = 'b' % { P("o") };
  c = 'c' % { P("p") };
  d = 'd' % { P("q") };
  e = 'e' % { P("r") };
  f = 'f' % { P("s") };
  g = 'g' % { P("t") };
  h = 'h' % { P("u") };
  i = 'i' % { P("v") };
  j = 'j' % { P("w") };
  k = 'k' % { P("x") };
  l = 'l' % { P("y") };
  m = 'm' % { P("z") };
  n = 'n' % { P("a") };
  o = 'o' % { P("b") };
  p = 'p' % { P("c") };
  q = 'q' % { P("d") };
  r = 'r' % { P("e") };
  s = 's' % { P("f") };
  t = 't' % { P("g") };
  u = 'u' % { P("h") };
  v = 'v' % { P("i") };
  w = 'w' % { P("j") };
  x = 'x' % { P("k") };
  y = 'y' % { P("l") };
  z = 'z' % { P("m") };

  alphaChar = a|b|c|d|e|f|g|h|i|j|k|l|m|n|o|p|q|r|s|t|u|v|w|x|y|z
             |A|B|C|D|E|F|G|H|I|J|K|L|M|N|O|P|Q|R|S|T|U|V|W|X|Y|Z;
  other = (any - alphaChar) > print_rest;             

  rot13 = (alphaChar | other)*;
  
  main := rot13;
}%%

%% write data;

int main(int argc, char **argv) {

  PRE;

  while(fgets(buffer, sizeof(buffer), stdin)) {
    INIT_LINE;
    %% write init;
    %% write exec;

    if (p != pe) {
      FAIL;
    }
  }

  POST;
  
  return 0;
}
