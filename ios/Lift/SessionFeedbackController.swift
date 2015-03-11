import Foundation

class SessionFeedbackController : UIViewController {
    var exerciseSession: ExerciseSession? = nil
    
    func setExerciseSession(exerciseSession: ExerciseSession) {
        self.exerciseSession = exerciseSession
    }
    
    func sendFeedback(feedback: SessionFeedback) {
        println("I got \(feedback.description) about session: \(exerciseSession)")
        exerciseSession?.submitFeedback(feedback) { (_) in
            self.exerciseSession?.end(const(()))
            self.exerciseSession = nil
        }
        self.navigationController?.popToRootViewControllerAnimated(true)
    }
    @IBAction func sendPositveFeedback() {
        sendFeedback(.Postive)
    }
    @IBAction func sendNeutralFeedback() {
        sendFeedback(.Neutral)
    }
    
    @IBAction func sendNegativeFeedback() {
        sendFeedback(.Negative)
    }
    

}