package core;

import comms.CommunicationManager;
import service.TrackingComponent;

public class ATCSystem {

    // class attributes
    private AircraftRegistry aircraftregistry;
    private TrackingComponent trackingcomponent;
    private CommunicationManager communicationmanager ;


    // constructors
    public ATCSystem(AircraftRegistry aircraftregistry, TrackingComponent trackingcomponent, CommunicationManager communicationmanager ) {
        this.aircraftregistry = aircraftregistry;
        this.trackingcomponent = trackingcomponent;
        this.communicationmanager  = communicationmanager ;
    }

    // SET and GET methods
    public AircraftRegistry getAircraftregistry() {
        return this.aircraftregistry;
    }

    public void setAircraftregistry(AircraftRegistry value) {
        this.aircraftregistry = value;
    }

    public TrackingComponent getTrackingcomponent() {
        return this.trackingcomponent;
    }

    public void setTrackingcomponent(TrackingComponent value) {
        this.trackingcomponent = value;
    }

    public CommunicationManager getCommunicationmanager () {
        return this.communicationmanager ;
    }

    public void setCommunicationmanager (CommunicationManager value) {
        this.communicationmanager  = value;
    }

    // class operations (owned)

    public void start() {

        //Start of user code ATCSystem_body : method body preserved
        // TODO: implementazione
        //End of user code
    }

    public void stop() {

        //Start of user code ATCSystem_body : method body preserved
        // TODO: implementazione
        //End of user code
    }

    public void tick() {

        //Start of user code ATCSystem_body : method body preserved
        // TODO: implementazione
        //End of user code
    }

    // operations to implement (interfaces + abstract inherited)

}
