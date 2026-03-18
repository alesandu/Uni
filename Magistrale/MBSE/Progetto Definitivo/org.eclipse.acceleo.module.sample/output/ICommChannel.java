
package comms
//Start of user code custom imports - preserved on regeneration
import ... // add packages to be imported
//End of user code

public interface ICommChannel
{

    public void open();


    public void close();


    public void transmit(String payload);

}
