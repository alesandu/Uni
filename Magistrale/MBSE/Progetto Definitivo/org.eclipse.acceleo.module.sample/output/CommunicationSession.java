
package comms
//Start of user code custom imports - preserved on regeneration
import ... // add packages to be imported
//End of user code

public class CommunicationSession {

    //class attributes
    private SessionStatus status;
    private Integer startedAt;
    private Integer endedAt;
    public java.util.List<Message> message;



    //constructor
    public CommunicationSession(SessionStatus status, Integer startedAt, Integer endedAt, java.util.List<Message> message){
        this.status = status;
        this.startedAt = startedAt;
        this.endedAt = endedAt;
        this.message = message;
    }

    //SET and GET methods
    public SessionStatus getStatus(){
        return this.status;
    }

    public void setStatus(SessionStatus value){
        this.status = value;
    }


    public Integer getStartedAt(){
        return this.startedAt;
    }

    public void setStartedAt(Integer value){
        this.startedAt = value;
    }


    public Integer getEndedAt(){
        return this.endedAt;
    }

    public void setEndedAt(Integer value){
        this.endedAt = value;
    }


    public java.util.List<Message> getMessage(){
        return this.message;
    }

    public void setMessage(java.util.List<Message> value){
        this.message = value;
    }

    //class operations (owned)
    public void open(){
        //Start of user code CommunicationSession_open_body : method body preserved
        // TODO: corpo non implementato. Inserisci qui l’implementazione (questa sezione non verrà sovrascritta).
        
        //End of user code
    }


    public void close(){
        //Start of user code CommunicationSession_close_body : method body preserved
        // TODO: corpo non implementato. Inserisci qui l’implementazione (questa sezione non verrà sovrascritta).
        
        //End of user code
    }


    public void transmit(Message m){
        //Start of user code CommunicationSession_transmit_body : method body preserved
        // TODO: corpo non implementato. Inserisci qui l’implementazione (questa sezione non verrà sovrascritta).
        
        //End of user code
    }

    //operations to implement (interfaces + abstract inherited)

}
