
package domain
//Start of user code custom imports - preserved on regeneration
import ... // add packages to be imported
//End of user code

public class Commander extends Person {

    //class attributes
    private String licenseId;



    //constructor
    public Commander(String licenseId){
        this.licenseId = licenseId;
    }

    //SET and GET methods
    public String getLicenseId(){
        return this.licenseId;
    }

    public void setLicenseId(String value){
        this.licenseId = value;
    }

    //class operations (owned)

    //operations to implement (interfaces + abstract inherited)

}
