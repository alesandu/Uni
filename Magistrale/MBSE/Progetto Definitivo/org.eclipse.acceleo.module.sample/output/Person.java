
package domain
//Start of user code custom imports - preserved on regeneration
import ... // add packages to be imported
//End of user code

public class Person {

    //class attributes
    private String id;
    private String name;



    //constructor
    public Person(String id, String name){
        this.id = id;
        this.name = name;
    }

    //SET and GET methods
    public String getId(){
        return this.id;
    }

    public void setId(String value){
        this.id = value;
    }


    public String getName(){
        return this.name;
    }

    public void setName(String value){
        this.name = value;
    }

    //class operations (owned)

    //operations to implement (interfaces + abstract inherited)

}
