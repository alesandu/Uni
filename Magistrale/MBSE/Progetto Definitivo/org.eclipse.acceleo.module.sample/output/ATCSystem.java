
package core
//Start of user code custom imports - preserved on regeneration
import ... // add packages to be imported
//End of user code

public class ATCSystem {

    //class attributes
    public AircraftRegistry aircraftregistry;
    public TrackingComponent trackingcomponent;
    public CommunicationManager communicationmanager ;


    private CriticalityLevel criticality = CriticalityLevel.MEDIUM;

    //constructor
    public ATCSystem(AircraftRegistry aircraftregistry, TrackingComponent trackingcomponent, CommunicationManager communicationmanager ){
        this.aircraftregistry = aircraftregistry;
        this.trackingcomponent = trackingcomponent;
        this.communicationmanager  = communicationmanager ;
    }

    //SET and GET methods
    public AircraftRegistry getAircraftregistry(){
        return this.aircraftregistry;
    }

    public void setAircraftregistry(AircraftRegistry value){
        this.aircraftregistry = value;
    }


    public TrackingComponent getTrackingcomponent(){
        return this.trackingcomponent;
    }

    public void setTrackingcomponent(TrackingComponent value){
        this.trackingcomponent = value;
    }


    public CommunicationManager getCommunicationmanager (){
        return this.communicationmanager ;
    }

    public void setCommunicationmanager (CommunicationManager value){
        this.communicationmanager  = value;
    }

    //class operations (owned)
    public void start(){
        //Start of user code ATCSystem_start_body : method body preserved
        // TODO: corpo non implementato. Inserisci qui l’implementazione (questa sezione non verrà sovrascritta).
        
        //End of user code
    }


    public void stop(){
        //Start of user code ATCSystem_stop_body : method body preserved
        // TODO: corpo non implementato. Inserisci qui l’implementazione (questa sezione non verrà sovrascritta).
        
        //End of user code
    }


    public void tick(){
        //Start of user code ATCSystem_tick_body : method body preserved
        // TODO: corpo non implementato. Inserisci qui l’implementazione (questa sezione non verrà sovrascritta).
        
        //End of user code
    }

    //operations to implement (interfaces + abstract inherited)

}
