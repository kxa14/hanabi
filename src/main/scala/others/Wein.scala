package others

import java.time.LocalDate

object Wein {
  case class Education(date: LocalDate, level: String)
  case class Experience(date: LocalDate, subject: String)
  case class Skill(date: LocalDate, subject: String)
  case class Story(
      birthday: LocalDate,
      schoolLevel: Option[Education],
      experiences: Vector[Experience],
      skills: Vector[Skill]
  )

  /** State0 is a function type written in a general purpose
    * that receives with a parameter & return a Tuple.
    * Also can be written as a case class, State */
  type State0[S, A] = S => (A, S)

  /**
    * Goal1: Keep all the previous states
    * Goal2: See the final State after computation
    *
    * Introduce a State type with map & flatMap methods
    * to use & combine the results of the previous computations of the program.
    */
  case class State[S, A](run: S => (A, S)) {
    self =>
    def flatMap[B](f: A => State[S, B]): State[S, B] = {
      State(s => {
        val (a, s1) =
          self.run(
            s
          ) // call run will split State into Action & CurrentState, State1
        f(a).run(
          s1
        ) // then apply computation -> create new State, State2 -> State2 calls run, chain it with State1
      }) // a = Skill, f = addSkill(Skill),  s/self = oldStory , s1 = newStory
    }
//    def map[B](f: A => B): State[S, B] = {
//      State(s => {
//        val (a, s1) = self.run(s)
//        (f(a), s1)
//        //        f(self.run(s)._1) -> self.run(s)._2)
//      })
//    }
    def map[B](f: A => B): State[S, B] = flatMap(a => State.pure(f(a)))
    def map2[B, C](fb: State[S, B])(f: (A, B) => C): State[S, C] = ???
  }

  object State {

    /** pure passes through the State without using it,
      * and returns a constant value a with the unchanged state */
    def pure[S, A](a: A): State[S, A] = State(s => (a, s))

    /**
      * Every element in the list represents a new State.
      * In order to save the state of this list of result,
      * we’re going to have this function, it combines the states of every element.
      * @param fs
      * @tparam S
      * @tparam A
      * @return
      */
    def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = ???

    /** The birthday would be the same in all State,
      * so we need to read it from the state */
    def get[S]: State[S, S] = State(s => (s, s))
  }

  //  def makeStory(oldStory: Story): Story = ???
  //  val makeStory: Story => Story = oldStory => ???
  /** Age:
    * A function to compute current age.
    * Age changes every year => Story is updated => State changes
    */
  val makeStory0: Story => (Int, Story) = oldStory =>
    (LocalDate.now.getYear - oldStory.birthday.getYear, ???)

  val makeStory: State[Story, Int] =
    State(oldStory => (LocalDate.now.getYear - oldStory.birthday.getYear, ???))

  /** Functions to update Story & compute the Action which is an Option[Education]*/

  /** Experience:
    *
    */
  def addExperience(experience: Experience): State[Story, Option[Education]] =
    State(oldStory =>
      None -> oldStory.copy(experiences = oldStory.experiences :+ experience)
    )

  def updateEducation(
      maybeLevel: Option[Education]
  ): State[Story, Option[Education]] =
    State(oldStory => maybeLevel -> oldStory.copy(schoolLevel = maybeLevel))

  def addSkill(skill: Skill): State[Story, Option[Education]] =
    State(oldStory => None -> oldStory.copy(skills = oldStory.skills :+ skill))

  val initialStory: Story =
    Story(LocalDate.of(1990, 9, 22), None, Vector.empty, Vector.empty)

  /** Story has the following Events to create the Story (to update the state) */
  sealed trait Event
  case class GoToSchool(level: String) extends Event
  case object FinishSchool extends Event
  case class AddExperience(subject: String) extends Event
  case class AddSkill(subject: String) extends Event

  def makeStory(
      event: Event,
      date: LocalDate
  ): State[Story, Option[Education]] =
    event match {
      case GoToSchool(level)      => updateEducation(Some(Education(date, level)))
      case FinishSchool           => updateEducation(None)
      case AddExperience(subject) => addExperience(Experience(date, subject))
      case AddSkill(subject)      => addSkill(Skill(date, subject))
    }

}
