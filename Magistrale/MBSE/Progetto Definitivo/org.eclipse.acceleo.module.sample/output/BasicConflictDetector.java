
package service
//Start of user code custom imports - preserved on regeneration
import ... // add packages to be imported
//End of user code

public class BasicConflictDetector implements IConflictDetector {

    //class attributes
    private Double minHorizontalSep;
    private Double minVerticalSep;


    private CriticalityLevel criticality = CriticalityLevel.HIGH;

    //constructor
    public BasicConflictDetector(Double minHorizontalSep, Double minVerticalSep){
        this.minHorizontalSep = minHorizontalSep;
        this.minVerticalSep = minVerticalSep;
    }

    //SET and GET methods
    public Double getMinHorizontalSep(){
        return this.minHorizontalSep;
    }

    public void setMinHorizontalSep(Double value){
        this.minHorizontalSep = value;
    }


    public Double getMinVerticalSep(){
        return this.minVerticalSep;
    }

    public void setMinVerticalSep(Double value){
        this.minVerticalSep = value;
    }

    //class operations (owned)

    //operations to implement (interfaces + abstract inherited)
    @Override
    public java.util.List<Conflict> detect(java.util.List<Aircraft> aircrafts){
        //Start of user code BasicConflictDetector_body : method body preserved
        // TODO: corpo non implementato. Inserisci qui l’implementazione (questa sezione non verrà sovrascritta).
    	return null;
        //End of user code
    }

}
