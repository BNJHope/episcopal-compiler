import java.util.Random;
import org.apache.commons.math3.distribution.BetaDistribution;

public class Beta implements IDistribution {

    private Float alpha;

    private Float beta;

    private BetaDistribution dist;

    public Beta(Float alpha, Float beta) {
        this.alpha = alpha;
        this.beta = beta;
        this.dist = new BetaDistribution((double) alpha.floatValue(), (double) beta.floatValue());
    }

    public Float sample() {
        return new Float((float) this.dist.sample());
    }

    @Override
    public String toString() {
        return("Beta Alpha: " + this.alpha + " Beta: " + this.beta);
    }

}
