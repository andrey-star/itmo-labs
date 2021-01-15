import org.apache.commons.math3.linear.*;

public class LeastSquares {

	public double[] singularValueDecomposition(double[][] f, double[] y, double tau) {
		RealMatrix yMat = new Array2DRowRealMatrix(y);
		RealMatrix fMat = new Array2DRowRealMatrix(f);
		SingularValueDecomposition svd = new SingularValueDecomposition(fMat);
		
		RealMatrix u = svd.getV(); // USV -> VDU
		RealMatrix vt = svd.getUT(); // USV -> VDU
		RealMatrix dd = svd.getS();
		for (int i = 0; i < dd.getColumnDimension(); i++) {
			double lambda = dd.getEntry(i, i);
			dd.setEntry(i, i,lambda / (Math.pow(lambda, 2) + tau));
		}
		RealMatrix alpha = u.multiply(dd).multiply(vt).multiply(yMat);
		return alpha.getColumn(0);
	}

}
