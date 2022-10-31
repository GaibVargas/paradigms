import BoardsModule
import SolverModule

main = do
  print("Solucao do tabuleiro 6x6")
  mapM_ print (solver board6)
  print("Solucao do tabuleiro 10x10")
  mapM_ print (solver board10)