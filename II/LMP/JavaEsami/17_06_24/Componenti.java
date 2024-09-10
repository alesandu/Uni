package p170624;

//10:17
public class Componenti {
	private String nome;
	private String paeseProvenienza;
	private int tempiOrdinazione;
	private double costo;
	
    public Componenti(String nome, String paeseProvenienza, int tempiOrdinazione, double costo) {
		this.nome = nome;
		this.paeseProvenienza = paeseProvenienza;
		this.tempiOrdinazione = tempiOrdinazione;
		this.costo = costo;
	}

	public String getNome() {
		return nome;
	}

	public void setNome(String nome) {
		this.nome = nome;
	}

	public String getPaeseProvenienza() {
		return paeseProvenienza;
	}

	public void setPaeseProvenienza(String paeseProvenienza) {
		this.paeseProvenienza = paeseProvenienza;
	}

	public int getTempiOrdinazione() {
		return tempiOrdinazione;
	}

	public void setTempiOrdinazione(int tempiOrdinazione) {
		this.tempiOrdinazione = tempiOrdinazione;
	}

	public double getCosto() {
		return costo;
	}

	public void setCosto(double costo) {
		this.costo = costo;
	}
    
}