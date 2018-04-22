/**
 * Walker Sorlie and Aspen Hopkins
 * CMPT 352
 * April 2016
 *
 */

import java.io.*;
import java.net.*;
import java.util.Enumeration;
import java.util.Iterator;

public class BroadcastThread implements Runnable {

        public void run() {
            Enumeration<String> threadEnum;
            DataOutputStream toClients = null;

            while (true) {	
                // sleep for 1/10th of a second
                try {
                    Thread.sleep(100);
                }

                catch (InterruptedException ignore) { }
                /**
                 * check if there are any messages in the Vector. If so, remove them
                 * and broadcast the messages to the chatroom
                 */
                try {
                    while(!Server2.broadcastThread.isEmpty()) {
                        String message = Server2.broadcastThread.remove(0);
                        
                        for(DataOutputStream output:Server2.userList.values()) {
                            output.writeBytes(message);

                            System.out.println("<" + message + " was sent>");

                        }
                    }
                }

                catch(IOException e) {
                    // remove socket that caused IOException from list
//                    Server.userList.remove(Server.userList.get());		// how to get the socket?
                }
            }
        }
}
