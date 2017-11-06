public class ObservationResult {

    private Boolean isValid;

    private Float result;

    public ObservationResult() {
        this.isValid = new Boolean(false);
        this.result = new Float(0.0f);
    }

    public ObservationResult(Float result) {
        this.isValid = new Boolean(true);
        this.result = result;   
    }

    @Override
    public String toString() {
        return("Observation Result\nValid : " + this.isValid + "\nResult : " + this.result);
    }

}
