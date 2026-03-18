
package comms
//Start of user code custom imports - preserved on regeneration
import ... // add packages to be imported
//End of user code

public class VHFChannel implements ICommChannel {

    //class attributes
    private String channelId;
    private Double frequencyMHz;
    private ChannelStatus status;


    private CriticalityLevel criticality = CriticalityLevel.LOW;

    //constructor
    public VHFChannel(String channelId, Double frequencyMHz, ChannelStatus status){
        this.channelId = channelId;
        this.frequencyMHz = frequencyMHz;
        this.status = status;
    }

    //SET and GET methods
    public String getChannelId(){
        return this.channelId;
    }

    public void setChannelId(String value){
        this.channelId = value;
    }


    public Double getFrequencyMHz(){
        return this.frequencyMHz;
    }

    public void setFrequencyMHz(Double value){
        this.frequencyMHz = value;
    }


    public ChannelStatus getStatus(){
        return this.status;
    }

    public void setStatus(ChannelStatus value){
        this.status = value;
    }

    //class operations (owned)

    //operations to implement (interfaces + abstract inherited)
    @Override
    public void open(){
        //Start of user code VHFChannel_body : method body preserved
        // TODO: corpo non implementato. Inserisci qui l’implementazione (questa sezione non verrà sovrascritta).
        
        //End of user code
    }


    @Override
    public void transmit(String payload){
        //Start of user code VHFChannel_body : method body preserved
        // TODO: corpo non implementato. Inserisci qui l’implementazione (questa sezione non verrà sovrascritta).
        
        //End of user code
    }


    @Override
    public void close(){
        //Start of user code VHFChannel_body : method body preserved
        // TODO: corpo non implementato. Inserisci qui l’implementazione (questa sezione non verrà sovrascritta).
        
        //End of user code
    }

}
