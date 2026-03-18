package service;

import domain.Aircraft;
import java.util.List;

public class BasicConflictDetector implements IConflictDetector {

    // class attributes
    private Double minHorizontalSep;
    private Double minVerticalSep;


    // constructors
    public BasicConflictDetector(Double minHorizontalSep, Double minVerticalSep) {
        this.minHorizontalSep = minHorizontalSep;
        this.minVerticalSep = minVerticalSep;
    }

    // SET and GET methods
    public Double getMinHorizontalSep() {
        return this.minHorizontalSep;
    }

    public void setMinHorizontalSep(Double value) {
        this.minHorizontalSep = value;
    }

    public Double getMinVerticalSep() {
        return this.minVerticalSep;
    }

    public void setMinVerticalSep(Double value) {
        this.minVerticalSep = value;
    }

    // class operations (owned)

    // operations to implement (interfaces + abstract inherited)

    @Override
    public List<Conflict> detect(List<Aircraft> aircrafts) {

        //Start of user code BasicConflictDetector_body : method body preserved
        // TODO: implementazione
        //End of user code

        return null;
    }

}
