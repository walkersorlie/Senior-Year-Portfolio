/**
 * Walker Sorlie and Aspen Hopkins
 * CMPT 352
 * April 2016
 */

import java.net.*;
import java.io.*;
import java.util.concurrent.*;
import java.util.Vector;

public class Server2 {

        public static final int PORT = 1337;
        private static final Executor exec = Executors.newCachedThreadPool();		
        public static ConcurrentHashMap<String, DataOutputStream> userList = new ConcurrentHashMap<String, DataOutputStream>();		// key is the username, value is the DataOutputStream
        public static Vector<String> broadcastThread = new Vector<String>();

        public static void main(String[] args) throws IOException {
            ServerSocket server = null;

            try {
                Runnable broadcastTask = new BroadcastThread();
                exec.execute(broadcastTask);

                server = new ServerSocket(PORT);

                while (true) {
                    /** now listen for connections
                    * and service the connection in a separate thread
                    */
                    Socket socket = server.accept();
                    
                    Runnable clientThread = new ChatThread(socket);
                    exec.execute(clientThread);
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
}

