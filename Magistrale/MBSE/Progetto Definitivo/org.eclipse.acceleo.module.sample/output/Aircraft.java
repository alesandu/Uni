
package domain
//Start of user code custom imports - preserved on regeneration
import ... // add packages to be imported
//End of user code

public class Aircraft {

    //class attributes
    private String id;
    private Double altitude;
    private Double speed;
    private Integer lastUpdateTs;
    public Commander commander;
    public Position position;


    private CriticalityLevel criticality = CriticalityLevel.LOW;

    //constructor
    public Aircraft(String id, Double altitude, Double speed, Integer lastUpdateTs, Commander commander, Position position){
        this.id = id;
        this.altitude = altitude;
        this.speed = speed;
        this.lastUpdateTs = lastUpdateTs;
        this.commander = commander;
        this.position = position;
    }

    //SET and GET methods
    public String getId(){
        return this.id;
    }

    public void setId(String value){
        this.id = value;
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


    public Integer getLastUpdateTs(){
        return this.lastUpdateTs;
    }

    public void setLastUpdateTs(Integer value){
        this.lastUpdateTs = value;
    }


    public Commander getCommander(){
        return this.commander;
    }

    public void setCommander(Commander value){
        this.commander = value;
    }


    public Position getPosition(){
        return this.position;
    }

    public void setPosition(Position value){
        this.position = value;
    }

    //class operations (owned)
    public void updateState(Position pos, Double altitude, Double speed, Integer ts){
        //Start of user code Aircraft_updateState_body : method body preserved
        // TODO: corpo non implementato. Inserisci qui l’implementazione (questa sezione non verrà sovrascritta).
        
        //End of user code
    }

    //operations to implement (interfaces + abstract inherited)

}
