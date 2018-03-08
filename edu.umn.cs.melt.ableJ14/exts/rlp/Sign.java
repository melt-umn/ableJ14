package edu.umn.cs.melt.ableJ14.exts.rlp ;

public class Sign {

   public static int signMethod (double x) {
      int r ;
      if (x > 0)
         r = 1 ;
      else
      if (x < 0)
         r = 0 - 1;
      else
         r = 0;

      return r ;
   }

}
