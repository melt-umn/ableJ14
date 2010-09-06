/**
    Complex implements a complex number and defines complex
    arithmetic and mathematical functions
    Last Updated February 27, 2001
    Copyright 1997-2001
    @version 1.0
    @author Andrew G. Bennett
*/

package edu.umn.cs.melt.java14.exts.complex ;

public class PrimitiveComplex extends Object {

    private double real_part,imag_part;
    
    public PrimitiveComplex(double r,double i) {
        real_part=r;
        imag_part=i;
    }
    
    public double real() {
        return real_part;
    }
    
    public double imag() {
        return imag_part;
    }
        
    public PrimitiveComplex plus(PrimitiveComplex w) {
        return new PrimitiveComplex(real_part+w.real(),imag_part+w.imag());
    }
    
    public PrimitiveComplex minus(PrimitiveComplex w) {
        return new PrimitiveComplex(real_part-w.real(),imag_part-w.imag());
    }
    
    public PrimitiveComplex times(PrimitiveComplex w) {
        return new PrimitiveComplex(real_part*w.real()-imag_part*w.imag(),real_part*w.imag()+imag_part*w.real());
    }
    
    public String toString() {
        if (imag_part>=0) {
            return real_part+" + "+imag_part+"i";
        }
        else {
            return real_part+" - "+(-imag_part)+"i";
	}
    }       
}
