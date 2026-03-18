package comms;

import java.util.List;

public class CommunicationSession {

    // class attributes
    private SessionStatus status;
    private Integer startedAt;
    private Integer endedAt;
    private List<Message> message;


    // constructors
    public CommunicationSession(SessionStatus status, Integer startedAt, Integer endedAt, List<Message> message) {
        this.status = status;
        this.startedAt = startedAt;
        this.endedAt = endedAt;
        this.message = message;
    }

    // SET and GET methods
    public SessionStatus getStatus() {
        return this.status;
    }

    public void setStatus(SessionStatus value) {
        this.status = value;
    }

    public Integer getStartedAt() {
        return this.startedAt;
    }

    public void setStartedAt(Integer value) {
        this.startedAt = value;
    }

    public Integer getEndedAt() {
        return this.endedAt;
    }

    public void setEndedAt(Integer value) {
        this.endedAt = value;
    }

    public List<Message> getMessage() {
        return this.message;
    }

    public void setMessage(List<Message> value) {
        this.message = value;
    }

    // class operations (owned)

    public void open() {

        //Start of user code CommunicationSession_body : method body preserved
        // TODO: implementazione
        //End of user code
    }

    public void close() {

        //Start of user code CommunicationSession_body : method body preserved
        // TODO: implementazione
        //End of user code
    }

    public void transmit(Message m) {

        //Start of user code CommunicationSession_body : method body preserved
        // TODO: implementazione
        //End of user code
    }

    // operations to implement (interfaces + abstract inherited)

}
