/**
 * Walker Sorlie and Aspen Hopkins
 * CMPT 352
 * April 2016
 */

import java.net.*;
import java.io.*;
import java.util.concurrent.*;
import java.util.ArrayList;
import java.util.Vector;
import java.util.Iterator;

public class Server {

        public static final int PORT = 1337;
        private static final Executor exec = Executors.newCachedThreadPool();
        public static ArrayList<Socket> threadList = new ArrayList<Socket>();		
        public static ArrayList<String> users = new ArrayList<String>();			
        public static ConcurrentHashMap<String, DataOutputStream> userList = new ConcurrentHashMap<String, DataOutputStream>();		// key is the username, value is the socket; change socket to DataOutputStream
        public static Vector<String> broadcastThread = new Vector<String>();

        public static void main(String[] args) throws IOException {
            ServerSocket server = null;

            try {
                Runnable broadcastTask = new BroadcastThread();
                exec.execute(broadcastTask);

                // establish the socket
                server = new ServerSocket(PORT);

                while (true) {
                    /** now listen for connections
                    * and service the connection in a separate thread
                    */
                    Socket socket = server.accept();
                    
                    // new ChatThread object representing the client
                    ChatThread client = new ChatThread(socket);		// this doesn't actually do anything
                    
                    nameCheck(socket);
                    // checks the name and then uses Runnable
                }
            }
            catch (IOException e) {
                System.err.println(e);
            }

            finally {
                if (server!= null)
                    server.close();
            }
        }

        public static void nameCheck(Socket sock) {		// input is not read from the user
            BufferedReader clientReader = null;
            PrintWriter toClient = null;

            try {
                toClient = new PrintWriter(sock.getOutputStream());

                toClient.write("Please choose a username");

                clientReader = new BufferedReader(new InputStreamReader(sock.getInputStream()));
                String clientName = clientReader.readLine();
                
                clientName = clientName.substring(clientName.indexOf(" ") + 1);

                System.out.println("<" + clientName + ">");

//                if((!users.contains(clientName.toLowerCase())) || users.isEmpty() || !userList.containsKey(clientName)) {		// change here to check the hashmap 
				  if(!userList.containsKey(clientName)) {
                    // adds HashMap value
                    userList.put(clientName, new DataOutputStream(sock.getOutputStream()));
                    
                    sendNameList(sock, clientName);
                }

                else {
                    toClient.write("2\r\n");
                    toClient.flush();
                    sock.close();
                }
            }
            catch(java.io.IOException e) {
                System.err.println(e);
            }
        }


        public static void sendNameList (Socket sock, String clientName) {		// send user list using the hashmap; clean up code here 
            DataOutputStream toClient = null;
            Iterator<String> toClientIterator;
            try {
                toClient = new DataOutputStream(sock.getOutputStream());
                toClient.writeBytes("1" + " ");
                toClientIterator = Server.users.iterator();		

                while(toClientIterator.hasNext()) {
                    toClient.writeBytes("," + toClientIterator.next());		// change how commas sent; change my parsing in ChatScreen
//                    toClient.writeBytes(" " + "Welcome to the coolest server\r\n");
//                    broadcastThread.add("10 " + clientName +"\r\n");
//                    Runnable task = new Connection(sock, clientName);
//                    exec.execute(task);
                }
                
                toClient.writeBytes(" " + "Welcome to the coolest server\r\n");
                broadcastThread.add("10" + " " + clientName + "\r\n");
                Runnable task = new Connection(sock, clientName);
                exec.execute(task);
            }
            catch(java.io.IOException e) {
                System.err.println(e);
            }
        }
}

