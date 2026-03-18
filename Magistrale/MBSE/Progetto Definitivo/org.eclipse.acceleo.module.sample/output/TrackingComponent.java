
package service
//Start of user code custom imports - preserved on regeneration
import ... // add packages to be imported
//End of user code

public class TrackingComponent {

    //class attributes
    public IRadarSurveillance iradarsurveillance;
    public AircraftRegistry aircraftregistry;
    public IConflictDetector iconflictdetector;

    private Integer periodMs; //attributo dello stereotipo Periodic

    private CriticalityLevel criticality = CriticalityLevel.HIGH;

    //constructor
    public TrackingComponent(IRadarSurveillance iradarsurveillance, AircraftRegistry aircraftregistry, IConflictDetector iconflictdetector){
        this.iradarsurveillance = iradarsurveillance;
        this.aircraftregistry = aircraftregistry;
        this.iconflictdetector = iconflictdetector;
        this.periodMs = 1000;
    }

    //SET and GET methods
    public IRadarSurveillance getIradarsurveillance(){
        return this.iradarsurveillance;
    }

    public void setIradarsurveillance(IRadarSurveillance value){
        this.iradarsurveillance = value;
    }


    public AircraftRegistry getAircraftregistry(){
        return this.aircraftregistry;
    }

    public void setAircraftregistry(AircraftRegistry value){
        this.aircraftregistry = value;
    }


    public IConflictDetector getIconflictdetector(){
        return this.iconflictdetector;
    }

    public void setIconflictdetector(IConflictDetector value){
        this.iconflictdetector = value;
    }

    //class operations (owned)
    public void pollRadarAndUpdate(){
        //Start of user code TrackingComponent_pollRadarAndUpdate_body : method body preserved
        // TODO: corpo non implementato. Inserisci qui l’implementazione (questa sezione non verrà sovrascritta).
        
        //End of user code
    }


    public java.util.List<Conflict> detectConflicts(){
        //Start of user code TrackingComponent_detectConflicts_body : method body preserved
        // TODO: corpo non implementato. Inserisci qui l’implementazione (questa sezione non verrà sovrascritta).
        return null;
        //End of user code
    }

    //operations to implement (interfaces + abstract inherited)

}
