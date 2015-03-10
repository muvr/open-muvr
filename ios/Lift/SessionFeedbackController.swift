import Foundation

class SessionFeedbackController : UITableViewController {
    var exerciseSession: NSUUID? = nil
    
    func setExerciseSession(exerciseSession: NSUUID) {
        self.exerciseSession = exerciseSession
    }
    
    func sendFeedback(feedback: SessionFeedback) {
        print("I got \(feedback) about session: \(exerciseSession)")
        navigationController?.popToRootViewControllerAnimated(true)
    }
    
    @IBAction func sendNegativeFeedback() {
        
        sendFeedback(.Neutral)
    }
    @IBAction func sendNeutralFeedback() {
        print("I am neutral about session: \(exerciseSession)")
        sendFeedback(.Negative)
    }
    @IBAction func sendPositveFeedback() {
        print("I am happy with session: \(exerciseSession)")
        sendFeedback(.Postive)
    }
}