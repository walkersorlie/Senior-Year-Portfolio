/**
 * Walker Sorlie and Aspen Hopkins
 * CMPT 352
 * April 2016
 *
 * This is the seperate thread
 * that services each incoming client request
 */

import java.net.*;
import java.io.*;

public class Connection implements Runnable {

        private Socket	client;
        private Handler handler = new Handler();
        private ChatThread chatThread;

        public Connection(Socket client, ChatThread chatThread) {
            this.client = client;
            this.chatThread = chatThread;
        }

        /**
         * This method runs in each separate thread.
         */
        public void run() {
            handler.process(client, chatThread);
            // catch NullPointerException that is thrown when a thread closes?
            // maybe handle closing the thread here?
        }

}




