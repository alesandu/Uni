package ImpresaSportiva;

import java.time.LocalDate;
import java.util.HashMap;

public class Sportivi {
	private String nome;
	private String cognome;
	private HashMap<Integer,Integer> reti;
	private LocalDate data;
	private Tipologia tipo;
	private String numeroIscrizione;
	private int livelloStipendio;
	
	public String getNome() {
		return nome;
	}
	public void setNome(String nome) {
		this.nome = nome;
	}
	public String getCognome() {
		return cognome;
	}
	public void setCognome(String cognome) {
		this.cognome = cognome;
	}
	public HashMap<Integer, Integer> getReti() {
		return reti;
	}
	public void setReti(HashMap<Integer, Integer> reti) {
		this.reti = reti;
	}
	public LocalDate getData() {
		return data;
	}
	public void setData(LocalDate data) {
		this.data = data;
	}
	public Tipologia getTipo() {
		return tipo;
	}
	public void setTipo(Tipologia tipo) {
		this.tipo = tipo;
	}
	public String getNumeroIscrizione() {
		return numeroIscrizione;
	}
	public void setNumeroIscrizione(String numeroIscrizione) {
		this.numeroIscrizione = "TEAM_"+numeroIscrizione;
	}
	public int getLivelloStipendio() {
		return livelloStipendio;
	}
	public void setLivelloStipendio(int livelloStipendio) throws Exception {
		if(livelloStipendio < 1 || livelloStipendio >5) {
			throw new Exception("numero non corretto");
		}
		this.livelloStipendio = livelloStipendio;
	}
	
	public Sportivi(String nome, String cognome, HashMap<Integer,Integer> reti, LocalDate data, Tipologia tipo, String numeroIscrizione,
			int livelloStipendio) throws Exception {
		super();
		this.nome = nome;
		this.cognome = cognome;
		this.reti = reti;
		this.data = data;
		this.tipo = tipo;
		this.numeroIscrizione = "TEAM_" + numeroIscrizione;
		setLivelloStipendio(livelloStipendio);
	}
	
	
}
