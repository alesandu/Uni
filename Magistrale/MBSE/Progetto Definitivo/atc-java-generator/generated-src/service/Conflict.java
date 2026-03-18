package service;

import domain.Aircraft;

public class Conflict {

    // class attributes
    private Integer detectedAt;
    private Aircraft a1;
    private Aircraft a2;
    private ConflictType type;


    // constructors
    public Conflict(Integer detectedAt, Aircraft a1, Aircraft a2, ConflictType type) {
        this.detectedAt = detectedAt;
        this.a1 = a1;
        this.a2 = a2;
        this.type = type;
    }

    // SET and GET methods
    public Integer getDetectedAt() {
        return this.detectedAt;
    }

    public void setDetectedAt(Integer value) {
        this.detectedAt = value;
    }

    public Aircraft getA1() {
        return this.a1;
    }

    public void setA1(Aircraft value) {
        this.a1 = value;
    }

    public Aircraft getA2() {
        return this.a2;
    }

    public void setA2(Aircraft value) {
        this.a2 = value;
    }

    public ConflictType getType() {
        return this.type;
    }

    public void setType(ConflictType value) {
        this.type = value;
    }

    // class operations (owned)

    // operations to implement (interfaces + abstract inherited)

}
