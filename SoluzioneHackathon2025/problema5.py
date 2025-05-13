import os


class Solution:

    inputFolder = os.path.join("soluzioni", "input", "5")
    outputFolder = os.path.join("soluzioni", "output", "5")

    @staticmethod
    def solve(edges: list[list[str, int, str, str]], targetSum: int) -> int:
        tree = {}
        children = set()
        for name, value, right, left in edges:
            tree[name] = (int(value), right, left)
            if right: children.add(right)
            if left: children.add(left)

        prefix_sums = {0: 1}
        count = 0

        def dfs(node: str, curr_sum: int) -> None:
            nonlocal count
            node_info = tree.get(node)
            if node_info is None:
                return

            val, right, left = node_info
            curr_sum += val

            count += prefix_sums.get(curr_sum - targetSum, 0)
            prefix_sums[curr_sum] = prefix_sums.get(curr_sum, 0) + 1

            dfs(left, curr_sum)
            dfs(right, curr_sum)

            prefix_sums[curr_sum] -= 1

        for node in tree:
            if node not in children:
                dfs(node, 0)

        return count
        pass

    @staticmethod
    def loadInput(i: int) -> str:
        """
        Carica il file di input i-esimo, contenuto all'interno della cartella dei file input.
        Questo metodo deve restituire il valore da passare al metodo solve.
        """
        files = os.listdir(Solution.inputFolder)
        files.sort()
        edges = []
        targetSum = 0
        with open(os.path.join(Solution.inputFolder, files[i])) as file:
            for i, line in enumerate(file):
                if i == 0:
                    targetSum = int(line.strip())
                else:
                    edges.append(line.strip().split(", "))
        return edges, targetSum

    @staticmethod
    def loadOutput(i: int) -> str:
        """
        Carica il file di output i-esimo, contenuto all'interno della cartella dei file output.
        Questo metodo deve restituire il valore presente nel file di output.
        """
        files = os.listdir(Solution.outputFolder)
        files.sort()

        with open(os.path.join(Solution.outputFolder, files[i])) as file:
            for line in file:
                return int(line.strip())
