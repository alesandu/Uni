package domain;

public class Aircraft {

    // class attributes
    private String id;
    private Double altitude;
    private Double speed;
    private Integer lastUpdateTs;
    private Position position;
    private Commander commander;


    // constructors
    public Aircraft(String id, Double altitude, Double speed, Integer lastUpdateTs, Position position, Commander commander) {
        this.id = id;
        this.altitude = altitude;
        this.speed = speed;
        this.lastUpdateTs = lastUpdateTs;
        this.position = position;
        this.commander = commander;
    }

    // SET and GET methods
    public String getId() {
        return this.id;
    }

    public void setId(String value) {
        this.id = value;
    }

    public Double getAltitude() {
        return this.altitude;
    }

    public void setAltitude(Double value) {
        this.altitude = value;
    }

    public Double getSpeed() {
        return this.speed;
    }

    public void setSpeed(Double value) {
        this.speed = value;
    }

    public Integer getLastUpdateTs() {
        return this.lastUpdateTs;
    }

    public void setLastUpdateTs(Integer value) {
        this.lastUpdateTs = value;
    }

    public Position getPosition() {
        return this.position;
    }

    public void setPosition(Position value) {
        this.position = value;
    }

    public Commander getCommander() {
        return this.commander;
    }

    public void setCommander(Commander value) {
        this.commander = value;
    }

    // class operations (owned)

    public void updateState(Position pos, Double altitude, Double speed, Integer ts) {

        //Start of user code Aircraft_body : method body preserved
        // TODO: implementazione
        //End of user code
    }

    // operations to implement (interfaces + abstract inherited)

}
