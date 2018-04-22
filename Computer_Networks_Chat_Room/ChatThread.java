import java.io.*;
import java.net.*;
import java.util.Iterator;

public class ChatThread implements Runnable {

	public Socket socket = null;
	public DataOutputStream clientOutputStream = null;
	public BufferedReader clientReader = null;
	public String clientName;
	public Thread thread;

	public ChatThread(Socket sock) {
		try {
			this.socket = sock;
			this.clientOutputStream = new DataOutputStream(sock.getOutputStream());
			this.clientReader = new BufferedReader(new InputStreamReader(sock.getInputStream()));
		}

		catch (java.io.IOException e) {
		}
	}

	public Socket getSocket() {
		return socket;
	}

	public DataOutputStream getOutputStream() {
		return clientOutputStream;
	}

	public BufferedReader getBufferedReader() {
		return clientReader;
	}
	
	public String getName() {
		return clientName;
	}

	public void run() {
		try {
			clientName = clientReader.readLine();

			clientName = clientName.substring(clientName.indexOf(" ") + 1);
			
			clientName.trim();

			System.out.println("<ChatThread Line 41: " + clientName + ">");

			if (!Server2.userList.containsKey(clientName)) {
				// adds HashMap value
				Server2.userList.put(clientName, new DataOutputStream(socket.getOutputStream()));
				
				clientOutputStream.writeBytes("1" + " ");

				Iterator iterator = Server2.userList.entrySet().iterator();

				for (String key : Server2.userList.keySet()) {
						clientOutputStream.writeBytes(key);
						if(iterator.hasNext())
							clientOutputStream.writeBytes(",");
					}
				
                clientOutputStream.writeBytes(" " + "Welcome to the coolest server\r\n");
                Server2.broadcastThread.add("10" + " " + clientName + "\r\n");
                thread = new Thread(new Connection(socket, this));
                thread.run();
				}
				          
			else {
				clientOutputStream.writeBytes("2\r\n");
				clientOutputStream.flush();
				socket.close();
			}
		} 
		catch (java.io.IOException e) {
			System.err.println(e);
		}
	}
}
