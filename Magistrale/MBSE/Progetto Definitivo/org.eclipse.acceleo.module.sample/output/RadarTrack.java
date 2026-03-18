
package service
//Start of user code custom imports - preserved on regeneration
import ... // add packages to be imported
//End of user code

public class RadarTrack {

    //class attributes
    private String aircraftId;
    private Double altitude;
    private Double speed;
    private Integer timestamp;
    public Position position;


    private CriticalityLevel criticality = CriticalityLevel.LOW;

    //constructor
    public RadarTrack(String aircraftId, Double altitude, Double speed, Integer timestamp, Position position){
        this.aircraftId = aircraftId;
        this.altitude = altitude;
        this.speed = speed;
        this.timestamp = timestamp;
        this.position = position;
    }

    //SET and GET methods
    public String getAircraftId(){
        return this.aircraftId;
    }

    public void setAircraftId(String value){
        this.aircraftId = value;
    }


    public Double getAltitude(){
        return this.altitude;
    }

    public void setAltitude(Double value){
        this.altitude = value;
    }


    public Double getSpeed(){
        return this.speed;
    }

    public void setSpeed(Double value){
        this.speed = value;
    }


    public Integer getTimestamp(){
        return this.timestamp;
    }

    public void setTimestamp(Integer value){
        this.timestamp = value;
    }


    public Position getPosition(){
        return this.position;
    }

    public void setPosition(Position value){
        this.position = value;
    }

    //class operations (owned)

    //operations to implement (interfaces + abstract inherited)

}
