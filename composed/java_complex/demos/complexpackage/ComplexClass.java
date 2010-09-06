package complexpackage;

public class ComplexClass extends Object {

    private double real_part,imag_part;
    
    public ComplexClass(double r,double i) {
        real_part=r;
        imag_part=i;
    }
    
    public double real() {
        return real_part;
    }
    
    public double imag() {
        return imag_part;
    }
        
    public ComplexClass plus(ComplexClass w) {
        return new ComplexClass(real_part+w.real(),imag_part+w.imag());
    }
    
    public ComplexClass minus(ComplexClass w) {
        return new ComplexClass(real_part-w.real(),imag_part-w.imag());
    }
    
    public ComplexClass times(ComplexClass w) {
        return new ComplexClass(real_part*w.real()-imag_part*w.imag(),real_part*w.imag()+imag_part*w.real());
    }
    
    public String toString() {
        if (imag_part>=0) {
            return real_part+" + "+imag_part+"i";
        }
        else {
            return real_part+" - "+(-imag_part)+"i";
	}
    }       

    public ComplexClass clone() {
        return new ComplexClass(real_part,imag_part);
    }
    
}
