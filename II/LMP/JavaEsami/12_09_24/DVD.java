package esame1;
import java.util.ArrayList;

public class DVD extends Supporto{
	
	private int durata;

	public int getDurata() {
		return durata;
	}

	public void setDurata(int durata) {
		this.durata = durata;
	}

	public DVD(String titolo, String entePubblicante, int annoPubblicazione, ArrayList<Prestito> prestiti, int durata) {
		super(titolo, entePubblicante, annoPubblicazione, prestiti);
		this.durata = durata;
	}
	
	
}