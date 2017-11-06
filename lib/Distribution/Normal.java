import java.util.Random;
import org.apache.commons.math3.distribution.NormalDistribution;

public class Normal implements IDistribution {

    private Float mean;

    private Float stanDev;

    private NormalDistribution dist;

    public Normal(Float mean, Float stanDev) {
        this.mean = mean;
        this.stanDev = stanDev;
        this.dist = new NormalDistribution((double) mean.floatValue(), (double) stanDev.floatValue());
    }

    public Float sample() {
        return new Float((float) this.dist.sample());
    }

    @Override
    public String toString() {
        return("Normal Mean: " + this.mean + " Standard Deviation: " + this.stanDev);
    }

}
