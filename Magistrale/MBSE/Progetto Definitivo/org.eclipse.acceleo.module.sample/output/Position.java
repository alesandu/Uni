
package domain
//Start of user code custom imports - preserved on regeneration
import ... // add packages to be imported
//End of user code

public class Position {

    //class attributes
    private Double latitude;
    private Double longitude;



    //constructor
    public Position(Double latitude, Double longitude){
        this.latitude = latitude;
        this.longitude = longitude;
    }

    //SET and GET methods
    public Double getLatitude(){
        return this.latitude;
    }

    public void setLatitude(Double value){
        this.latitude = value;
    }


    public Double getLongitude(){
        return this.longitude;
    }

    public void setLongitude(Double value){
        this.longitude = value;
    }

    //class operations (owned)

    //operations to implement (interfaces + abstract inherited)

}
