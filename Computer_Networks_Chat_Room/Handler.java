/**
 * Walker Sorlie and Aspen Hopkins
 * CMPT 352
 * April 2016
 */

import java.io.*;
import java.net.*;
import java.util.TimeZone;
import java.util.Date;
import java.text.DateFormat;
import java.text.SimpleDateFormat;

public class Handler {
	
        BufferedReader fromThread = null;
        DataOutputStream toClient = null;
        private static boolean STILL_CONNECTED = true;

        /**
         * this method is invoked by a separate thread
         *
         * Run message handling in here
         */
        public void process(Socket client, ChatThread chatThread) {

            try {
                fromThread = new BufferedReader(new InputStreamReader(client.getInputStream()));
                toClient = new DataOutputStream(client.getOutputStream());

                // when to set character encoding?
                while(STILL_CONNECTED) {
                    String clientInput = chatThread.getBufferedReader().readLine();					
                    String command = clientInput.substring(0, 1);

					// client sends general message to server
                    if(command.equals("3")) {		// get name of person who posted message 		
						String message = clientInput.substring(1);
						Server2.broadcastThread.add("5 " + chatThread.getName() + " " + getDatetimeGMT() + " " + message + "\r\n");		// username in message?
						
						System.out.println("<Line 41, Command 3:" + "5 " + chatThread.getName() + " " + getDatetimeGMT() + " " + message + "\r\n" + ">");
                    }
                    
                    // client sends private message to server
                    else if(command.equals("4")) {
						// gets a string without the command number and the first space
						String withoutCommandNumber = clientInput.substring(clientInput.indexOf(" ") + 1);
						
						System.out.println("<Handler Line 49 Private Message withoutCommandNumber:" + withoutCommandNumber + ">");
						
						// gets who the message is from
						String fromUsername = withoutCommandNumber.substring(0, withoutCommandNumber.indexOf(" "));
						
						System.out.println("<Handler Line 54 Private Message fromUsername:" + fromUsername + ">");
						
						// gets who the message is for
						String toUsername = withoutCommandNumber.substring((withoutCommandNumber.indexOf(" ") + 1), withoutCommandNumber.indexOf(" ", (withoutCommandNumber.indexOf(" ") + 1)));		// gets who message is to
						
						System.out.println("<Handler Line 59 Private Message toUsername:" + toUsername + ">");
						
						// holder to get the actual message
                        String holder = withoutCommandNumber.substring(withoutCommandNumber.indexOf(" ") + 1);;
						String message = holder.substring(holder.indexOf(" ") + 1);
						
						System.out.println("<Handler Line 65 Private Message message:" + message + ">");
						
//						Socket toUsernameSocket = Server.userList.get(toUsername);
//						toClient = new DataOutputStream(toUsernameSocket.getOutputStream());
						toClient = Server2.userList.get(toUsername);
						toClient.writeBytes("6" + " " + fromUsername + " " + toUsername + " "+ getDatetimeGMT() + " " + message + "\r\n");						
                    }
                    
                    // client sends a disconnect request
                    else if(command.equals("7")) {		
						STILL_CONNECTED = false;	// if user sends a disconnect request, this will break out of while loop
                    }
                }
                
                // closing the connection
                toClient.writeBytes("8\r\n");
                toClient.close();
                fromThread.close();
                client.close(); 
                
                // this is the message sent to all clients for a client who left
                Server2.broadcastThread.add("9" + " " +chatThread.getName() + "\r\n");		
				
				// HashMap remove
				Server2.userList.remove(chatThread.getName());
				
            }
            catch(java.io.IOException e) {
                System.err.println(e);
            }
        }

        // helper method to get current GMT datetime
        private static String getDatetimeGMT() {
            DateFormat dateFormat = new SimpleDateFormat("yyyy:MM:dd:HH:mm:ss");
            Date date = new Date();
            dateFormat.setTimeZone(TimeZone.getTimeZone("GMT"));
            return dateFormat.format(date);
        }
}




