//package aula12;

import java.util.Random;

class FilaSegura implements Runnable{

	Random r = new Random();
	Fila filaTest = new Fila();

	class Fila{

		Node head;


		void inserir(int val){
			if (head == null) {this.head = new Node(val);
			}
			else{
				Node aux = head;
				while(aux != null){
					aux = aux.next;
				}
				aux = new Node(val);
				System.out.println("inseriu "+val);
			}
		}

		void remover(){
			if (head != null){
				Node aux = this.head;
				System.out.println("removey "+aux.val);
				this.head = aux.next;
				aux = null;
			}
		}

	}
	class Node{
		int val;
		Node next;

		public Node(int val) {
			// TODO Auto-generated constructor stub
			this.val = val;
		}

	}

	synchronized void inserirLoop(){
		int v = 0;

		for (int i = 0; i < 10; i++) {
			v = r.nextInt(1000)+1;
			synchronized(this){
				filaTest.inserir(v);
			}

		}
	}

	synchronized void removerLoop(){

		for (int i = 0; i < 8; i++) {
			
				filaTest.remover();
			
		}

	}

	@Override
	public void run() {
		// TODO Auto-generated method stub
		inserirLoop();
		removerLoop();

	}


	public static void main(String[] args) throws InterruptedException {

		FilaSegura filaSegura = new FilaSegura();
		Thread t;
		for (int i = 0; i < 8; i++) {
			t = new Thread(filaSegura);
			t.start();
			t.join();

		}

	}

}
