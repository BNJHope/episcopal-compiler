package Distribution;

import java.util.Random;

public class Flip implements IDistribution {

    private Float p;

    public Flip(Float p) {
        this.p = p;
    }

    public Float sample() {
        Random randGen = new Random();
        float result = randGen.nextFloat();
        return (result < p) ? new Float(1.0f) : new Float(0.0f);
    }

}
