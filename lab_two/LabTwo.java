import java.util.ArrayList;

public class LabTwo {
    public static void main(String[] args) {

        int numOfLists, numOfElements, valueInc;
        //changes string arguments from command line into ints
        try{
            numOfLists = Integer.parseInt(args[0]);
            numOfElements = Integer.parseInt(args[1]);
            valueInc = Integer.parseInt(args[2]);
            //ensures program only runs if values are acceptable
            //catches any errors and ends program gracefully
            try{
                if (numOfLists >=0 && numOfElements >= 0 && valueInc >= 0){
                    System.out.print(listOfLists(numOfLists,numOfElements,valueInc));
                }
                else{
                    System.out.println("Input cannot be negative.");
                    System.exit(0);
                }
            }
            catch (Exception e){
                System.out.print("Invalid input to produce a list of lists."); 
            }
        }
        catch(Exception e){
            System.out.println("Input must be an integer.");
            System.exit(0);
        }  
    }
        
        

    public static ArrayList<ArrayList<Integer>> listOfLists(int numOfLists, int numOfElements, int valueInc){
        //creates final list that all lists will be put into
        ArrayList<ArrayList<Integer>> finalList = new ArrayList<>();

        //loops until all lists within the list have been created
        while (numOfLists > 0){
            //creates the list
            ArrayList<Integer> innerList = new ArrayList<>();

            if(numOfElements != 0){ //used so if number of elements are 0 no value will be placed in list
                innerList.add(numOfLists); 
            }

            int tracker = numOfLists; //done so value of numOfLists doesnt change in inner loop
            //compiles each inner list
            for (int i = 1; i < numOfElements; i++){
                //upon each iteration, adds a new value to the list
                tracker += valueInc;
                innerList.add(tracker);
            }

            finalList.add(innerList);

            //after each loop, the number of lists left to add decreases by one
            numOfLists -= 1;


        }

        //returns the correct data type to orginal function to be printed
        return finalList;
    }
    
}