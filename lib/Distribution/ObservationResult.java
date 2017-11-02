package Distribution;

public class ObservationResult {

    private Boolean isValid;

    private Float result;

    public ObservationResult() {
        this.isValid = new Boolean(false);
        this.result = new Float(0.0f);
    }

    public ObservationResult(Boolean isValid, Float result) {
        this.isValid = isValid;
        this.result = result;   
    }

    public Boolean getIsValid() {
        return this.isValid;
    };

    public Float getResult() {
        return this.result;
    };

    public void setIsValid(Boolean isValid) {
        this.isValid = isValid;
    };

    public void setResult(Float result) {
        this.result = result;
    };


}
