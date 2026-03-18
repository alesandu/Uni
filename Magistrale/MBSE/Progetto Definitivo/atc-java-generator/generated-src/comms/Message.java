package comms;

public class Message {

    // class attributes
    private String text;
    private Integer timestamp;


    // constructors
    public Message(String text, Integer timestamp) {
        this.text = text;
        this.timestamp = timestamp;
    }

    // SET and GET methods
    public String getText() {
        return this.text;
    }

    public void setText(String value) {
        this.text = value;
    }

    public Integer getTimestamp() {
        return this.timestamp;
    }

    public void setTimestamp(Integer value) {
        this.timestamp = value;
    }

    // class operations (owned)

    // operations to implement (interfaces + abstract inherited)

}
