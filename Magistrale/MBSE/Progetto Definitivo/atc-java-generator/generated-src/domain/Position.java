package domain;

public class Position {

    // class attributes
    private Double latitude;
    private Double longitude;


    // constructors
    public Position(Double latitude, Double longitude) {
        this.latitude = latitude;
        this.longitude = longitude;
    }

    // SET and GET methods
    public Double getLatitude() {
        return this.latitude;
    }

    public void setLatitude(Double value) {
        this.latitude = value;
    }

    public Double getLongitude() {
        return this.longitude;
    }

    public void setLongitude(Double value) {
        this.longitude = value;
    }

    // class operations (owned)

    // operations to implement (interfaces + abstract inherited)

}
