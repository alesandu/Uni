package esame1;


import java.util.ArrayList;

public class Libro extends Supporto{

	private int numeroPagine;

	public int getNumeroPagine() {
		return numeroPagine;
	}

	public void setNumeroPagine(int numeroPagine) {
		this.numeroPagine = numeroPagine;
	}

	public Libro(String titolo, String entePubblicante, int annoPubblicazione, ArrayList<Prestito> prestiti,
			int numeroPagine) {
		super(titolo, entePubblicante, annoPubblicazione, prestiti);
		this.numeroPagine = numeroPagine;
	}

	
}
