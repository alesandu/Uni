package service;

import core.AircraftRegistry;
import java.util.List;

public class TrackingComponent {

    // class attributes
    private IRadarSurveillance iradarsurveillance;
    private AircraftRegistry aircraftregistry;
    private IConflictDetector iconflictdetector;


    // constructors
    public TrackingComponent(IRadarSurveillance iradarsurveillance, AircraftRegistry aircraftregistry, IConflictDetector iconflictdetector) {
        this.iradarsurveillance = iradarsurveillance;
        this.aircraftregistry = aircraftregistry;
        this.iconflictdetector = iconflictdetector;
    }

    // SET and GET methods
    public IRadarSurveillance getIradarsurveillance() {
        return this.iradarsurveillance;
    }

    public void setIradarsurveillance(IRadarSurveillance value) {
        this.iradarsurveillance = value;
    }

    public AircraftRegistry getAircraftregistry() {
        return this.aircraftregistry;
    }

    public void setAircraftregistry(AircraftRegistry value) {
        this.aircraftregistry = value;
    }

    public IConflictDetector getIconflictdetector() {
        return this.iconflictdetector;
    }

    public void setIconflictdetector(IConflictDetector value) {
        this.iconflictdetector = value;
    }

    // class operations (owned)

    public void pollRadarAndUpdate() {

        //Start of user code TrackingComponent_body : method body preserved
        // TODO: implementazione
        //End of user code
    }

    public List<Conflict> detectConflicts() {

        //Start of user code TrackingComponent_body : method body preserved
        // TODO: implementazione
        //End of user code

        return null;
    }

    // operations to implement (interfaces + abstract inherited)

}
