# A NETLIST SIMULATOR

[see](https://github.com/MrBigoudi/ProjetSysNumENS.git)


## HOWTO

---

To see how to run and try the simulator, chek the [HOWTO.md](HOWTO.md) file


## BEHAVIOUR


---

<ul>

<li>The simulator is interpreting every equations from the netlist at each steps (or infinitely if no number of steps was specified) using environments to spread infos between loops.</li>


<li>The environments are tables mapping identifiers to their current value.</li>

<li>We're using two different environments, one current and one representing the environment state in the last step. Doing so we can manage the registers' one step delay.</li>

<li>To interpret each equations we devide it into its identifiers and its expression</li>

<li>We then interpret the so call expression using the <code>calculExp</code> function which given an expression call the corresponding function that interprets this kind of expression and update the given environment.

<li>We've choosed to give a fixed adress size of 4 bits for both the RAM's and the ROM's adresses.</li>

</ul>


## DIFFICULTIES


---

It took me some time to understand properly how to manage the RAM and the ROM. 
For example, I couldn't get how to handle the property of the ROM which would have to be empty every time unless explicitely generated before the simulation.