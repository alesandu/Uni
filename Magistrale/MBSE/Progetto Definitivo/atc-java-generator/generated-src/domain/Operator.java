package domain;

public class Operator extends Person {

    // class attributes
    private String towerPosition;


    // constructors
    public Operator(String towerPosition) {
        this.towerPosition = towerPosition;
    }

    // SET and GET methods
    public String getTowerPosition() {
        return this.towerPosition;
    }

    public void setTowerPosition(String value) {
        this.towerPosition = value;
    }

    // class operations (owned)

    // operations to implement (interfaces + abstract inherited)

}
