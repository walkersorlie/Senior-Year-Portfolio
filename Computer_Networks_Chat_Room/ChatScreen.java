/**
 * This program is a rudimentary demonstration of Swing GUI programming.
 * Note, the default layout manager for JFrames is the border layout. This
 * enables us to position containers using the coordinates South and Center.
 *
 * Usage:
 *	java ChatScreen
 *
 * When the user enters text in the textfield, it is displayed backwards
 * in the display area.
 */

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.border.*;
import java.net.*;
import java.io.*;
import java.util.Scanner;
import java.util.ArrayList;

public class ChatScreen extends JFrame implements ActionListener, KeyListener {
	private JButton sendButton;
	private JButton exitButton;
	public JTextField sendText;
	public JTextArea displayArea;
	private static final int PORT = 1337;
	public static BufferedReader serverReader = null;
	public static DataOutputStream toServer = null;
	private static String userName;
	private static ArrayList<String> usernameList = new ArrayList<String>();
	public static Socket server = null;


	public ChatScreen() {
		/**
		 * a panel used for placing components
		 */
		JPanel p = new JPanel();

		Border etched = BorderFactory.createEtchedBorder();
		Border titled = BorderFactory.createTitledBorder(etched,"Enter Message Here ...");
		p.setBorder(titled);

		/**
		 * set up all the components
		 */
		sendText = new JTextField(30);
		sendButton = new JButton("Send");
		exitButton = new JButton("Exit");

		/**
		 * register the listeners for the different button clicks
		 */
		sendText.addKeyListener(this);
		sendButton.addActionListener(this);
		exitButton.addActionListener(this);

		/**
		 * add the components to the panel
		 */
		p.add(sendText);
		p.add(sendButton);
		p.add(exitButton);

		/**
		 * add the panel to the "south" end of the container
		 */
		getContentPane().add(p, "South");

		/**
		 * add the text area for displaying output. Associate a scrollbar with
		 * this text area. Note we add the scrollpane to the container, not the
		 * text area
		 */
		displayArea = new JTextArea(15, 40);
		displayArea.setEditable(false);
		displayArea.setFont(new Font("SansSerif", Font.PLAIN, 14));

		JScrollPane scrollPane = new JScrollPane(displayArea);
		getContentPane().add(scrollPane, "Center");

		/**
		 * set the title and size of the frame
		 */
		setTitle(userName + "'s Chat Room");
		pack();

		setVisible(true);
		sendText.requestFocus();

		(new Thread(new ReaderThread())).start();
		
		displayArea.append(usernameList.toString());

		/** anonymous inner class to handle window closing events */
		addWindowListener(new WindowAdapter() {
			public void windowClosing(WindowEvent evt) {
				System.exit(0);
			}
		});

	}

	/**
	 * Private class that represents the thread that reads messages from the
	 * server and displays the text to the screen
	 */
	private class ReaderThread implements Runnable {

		public void run() {
			try {
				String message;

				while (true) { // protocol 5
					// read from the socket
					message = serverReader.readLine();
					String command = message.substring(0, 1);

					System.out.println("<Line 116 " + message + ">\r\n");
					System.out.println("<Line 117 " + command + ">\r\n");

					if (command.equals("1")) { // userlist here?
						System.out.println("<Username accepted>\r\n");
					}

					// private message
					else if (command.equals("6")) {
						// gets the string without the command number
						String withoutCommand = message.substring(message.indexOf(" ") + 1);

						// gets who the message is from
//						String fromUsername = message.substring(message.indexOf(" "),(message.indexOf(" ",message.indexOf(" ") + 1)));
						String fromUsername = withoutCommand.substring(0, withoutCommand.indexOf(" "));
						
						System.out.println("<ChatScreen Line 132 fromUsername:" + fromUsername + ">");

						// gets who the message is to (me)
						String toUsername = withoutCommand.substring((withoutCommand.indexOf(" ") + 1),withoutCommand.indexOf(" ",(withoutCommand.indexOf(" ") + 1)));
						
						System.out.println("<ChatScreen Line 137 toUsername:" + toUsername + ">");

						// holder to get the timestamp and message
						String holder = withoutCommand.substring(withoutCommand.indexOf(" ", (withoutCommand.indexOf(" ") + 1)));
						
						System.out.println("<ChatScreen Line 142 holder:" + holder + ">");
						
						holder.trim();
						String finalMessage = holder.substring(holder.indexOf(" ", (holder.indexOf(" ") + 1)));
						
						System.out.println("<ChatScreen Line 146 message:" + finalMessage + ">");

						displayArea.append("Private message from " + fromUsername + ": " + finalMessage + "\n");
						sendText.setText("");
					} 
					
					else if (command.equals("10")) {
						String connectedPerson = message.substring(message.indexOf(" ") + 1);
						displayArea.append(connectedPerson + " connected\n");
						sendText.setText("");
						sendText.requestFocus();
					}
					// handles the user list
					
					// else if(command.equals("1")) {
					// String users = message.substring(message.indexOf(" "),
					// message.indexOf(" ", (message.indexOf(" ") + 1)));
					// String finalMessage =
					// message.substring(message.indexOf(" ",
					// (message.indexOf(" ") + 1)));

					// displayArea.append(users + "\n");
					// displayArea.append(finalMessage + "\n");
					// sendText.setText("");
					// sendText.requestFocus();
					// }

					// general message
					else if (command.equals("5")) {
						String withoutCommand = message.substring(message.indexOf(" ") + 1);

						System.out.println("<ReaderThread in ChatScreen Line 168:"+ withoutCommand + ">");

						String fromUser = withoutCommand.substring(0,withoutCommand.indexOf(" "));

						System.out.println("<ReaderThread in ChatScreen Line 172:"+ fromUser + ">");

						String finalMessage = withoutCommand.substring(withoutCommand.indexOf(" ",(withoutCommand.indexOf(" ") + 1))); 
																				
						displayArea.append(fromUser + ": " + finalMessage+ "\n");
					}
				}
			} catch (java.io.IOException ioe) {
			}
		}
	}

	/**
	 * This gets the text the user entered and outputs it in the display area.
	 */
	public void displayText() {
		try {
			String message = sendText.getText();
			
			System.out.println("<ChatScreen Line 198:" + message + ">");

			if (message.startsWith("whisper")) { // whisper 'username' message
				String toUsername = message.substring(message.indexOf(" "),message.indexOf(" ", (message.indexOf(" ") + 1)));
				String finalMessage = message.substring(message.indexOf(" ",(message.indexOf(" ") + 1))).trim();
				
				System.out.println("<ChatScreen Line 204 Private Message:4 " + userName + " " + toUsername + " " + finalMessage + ">");
				
				toServer.writeBytes("4" + " " + userName.trim() + " " + toUsername.trim() + " " + finalMessage + "\r\n");
				
				sendText.setText("");
			} 
			
			else {
				toServer.writeBytes("3" + message + "\r\n");

				System.out.println("<DisplayText in ChatScreen Protocol 3:" + message + ">");

				sendText.setText("");
			}

			// check here if the text the user enters is a private message or a
			// general message
			// establish what we want to do for private chat

		} 
		
		catch (java.io.IOException e) {
		}
	}

	/**
	 * This method responds to action events .... i.e. button clicks and
	 * fulfills the contract of the ActionListener interface.
	 */
	public void actionPerformed(ActionEvent evt) { 													
		Object source = evt.getSource(); 

		if (source == sendButton)
			displayText();
		else if (source == exitButton) {		// implement java.net.SocketException
			try {
				toServer.writeBytes("7\r\n");
				
				server.close();
				serverReader.close();
				toServer.close();
			} 
			
			catch (java.io.IOException e) {
			}
			System.exit(0);
		}
	}

	/**
	 * These methods responds to keystroke events and fulfills the contract of
	 * the KeyListener interface.
	 */

	/**
	 * This is invoked when the user presses the ENTER key.
	 */
	public void keyPressed(KeyEvent e) {
		if (e.getKeyCode() == KeyEvent.VK_ENTER)
			displayText();
	}

	/** Not implemented */
	public void keyReleased(KeyEvent e) {
	}

	/** Not implemented */
	public void keyTyped(KeyEvent e) {
	}

	public static boolean chat(String serverAddress) {
		try {
			server = new Socket(InetAddress.getByName(serverAddress), PORT);
			toServer = new DataOutputStream(server.getOutputStream());
			Scanner sc = new Scanner(System.in);
			userName = sc.nextLine();

			System.out.println("<" + userName + ">");

			toServer.writeBytes("0" + " " + userName + "\r\n");

			serverReader = new BufferedReader(new InputStreamReader(server.getInputStream()));
			String message = serverReader.readLine();
			String commandNumber = message.substring(0, 1);

			if (commandNumber.equals("2")) {
				System.out.println("Username already taken. Disconnected from client.");
				return false;
			} 
			
			else if (commandNumber.equals("1")) {
				// handle what to do with username list here
				// loop to find commas and seperate names
				// String.split()?
				// String.replaceAll()?
				String usernames = message.substring(message.indexOf(" "), message.indexOf(" ", (message.indexOf(" ") + 1)));
				usernames.concat(",");
				String username;
				
				for(int i = 0; i < usernames.length(); i = (usernames.indexOf(",")) + 1) {
					username = usernames.substring(usernames.indexOf(i), usernames.indexOf(","));
					usernameList.add(username);				
				}
				return true;
			}
		}
		
		catch (java.io.IOException e) {
			System.err.println(e);
		}

		return false;
	}

	public static void main(String[] args) { 
												
		boolean canContinue = chat(args[0]); 
												
		// what to do when a user leaves?
		if (canContinue) {
			JFrame win = new ChatScreen();
		}
	}
}
