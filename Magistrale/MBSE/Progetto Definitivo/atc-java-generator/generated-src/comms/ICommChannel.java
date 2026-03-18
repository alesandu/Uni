package comms;

public interface ICommChannel {

    public void open();

    public void close();

    public void transmit(String payload);

}
