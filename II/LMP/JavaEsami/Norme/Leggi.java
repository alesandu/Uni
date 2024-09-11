package Norme;
//10:37
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;

public class Leggi {
	
	private TipoLegge tipologia;
	private LocalDate data;

	private String identita;
	private String intestazione;
	private ArrayList<Articoli> articolata;
	private ArrayList<Object> allegato;
	private String conclusioni;
	
	public LocalDate getData() {
		return data;
	}
	public void setData(LocalDate data) {
		this.data = data;
	}
	public String getIdentita() {
		return identita;
	}
	public void setIdentita(String identita) {
		this.identita = identita;
	}
	public String getIntestazione() {
		return intestazione;
	}
	public void setIntestazione(String intestazione) {
		this.intestazione = intestazione;
	}
	public ArrayList<Articoli> getArticolata() {
		return articolata;
	}
	public void setArticolata(ArrayList<Articoli> articolata) {
		this.articolata = articolata;
	}
	
	
	public String getConclusioni() {
		return conclusioni;
	}
	public void setConclusioni(String conclusioni) {
		this.conclusioni = conclusioni;
	}
	
	public TipoLegge getTipologia() {
		return tipologia;
	}
	public void setTipologia(TipoLegge tipologia) {
		this.tipologia = tipologia;
	}
	
	public ArrayList<Object> getAllegato() {
		return allegato;
	}
	public void setAllegato(ArrayList<Object> allegato) {
		this.allegato = allegato;
	}
	
	public Leggi(TipoLegge tipologia, LocalDate data, String identita, String intestazione,
			ArrayList<Articoli> articolata, ArrayList<Object> allegato, String conclusioni) {
		super();
		this.tipologia = tipologia;
		this.data = data;
		this.identita = identita;
		this.intestazione = intestazione;
		this.articolata = articolata;
		this.allegato = allegato;
		this.conclusioni = conclusioni;
	}
	@Override
	public String toString() {
		DateTimeFormatter y = DateTimeFormatter.ofPattern("yy-mm-dd");
		data.format(y);
		return "legge: " + tipologia + " : " + data;
	}
	
}
