package p170624;

import java.util.ArrayList;
import java.util.HashMap;

public class Main {
    public static void main(String[] args) {
        Componenti componente1 = new Componenti("Componente1", "Italia", 5, 100.0, false);
        Componenti componente2 = new Componenti("Componente2", "Germania", 3, 150.0, true);

        ArrayList<Componenti> subcomponenti = new ArrayList<>();
        subcomponenti.add(componente1);
        subcomponenti.add(componente2);
        
        Prodotti prodotto1 = new Prodotti("ID1", "Prodotto1", subcomponenti, "Italia", 10, 1.5, 20.0);
        Prodotti prodotto2 = new Prodotti("ID2", "Prodotto2", subcomponenti, "Germania", 15, 1.2, 25.0);

        ArrayList<Prodotti> catena= new ArrayList<>();
        catena.add(prodotto1);
        catena.add(prodotto2);
        Manager manager = new Manager(catena);

        manager.setFattoreGuadagno(1.7);
        manager.setCostoManodopera(30.0);

        HashMap<Double, Prodotti> ranking = manager.getRanking();
        for (Double key : ranking.keySet()) {
            System.out.println("Guadagno/Tempo: " + key + " -> " + ranking.get(key).toString());
        }

        manager.presentazione();
    }
}
