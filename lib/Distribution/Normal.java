import java.util.Random;

public class Normal implements IDistribution {

    private Float mean;

    private Float stanDev;

    public Normal(Float mean, Float stanDev) {
        this.mean = mean;
        this.stanDev = stanDev;
    }

    public Float sample() {
        Random randGen = new Random();
        float result = randGen.nextFloat();
        return result;
    }

}
