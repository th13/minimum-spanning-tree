both: Prim Kruskal

Prim: MST/Prim.hs prim.hs
	ghc --make prim.hs -o Prim

Kruskal: MST/Kruskal.hs kruskal.hs
	ghc --make kruskal.hs -o Kruskal

clean:
	rm *.o *.hi MST/*.o MST/*.hi Kruskal Prim
