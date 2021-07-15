package codecs.xml

import codecs.DecodeResult.Ok
import codecs.xml.internal.CodecAttribute
import org.scalatest.{FreeSpec, Matchers}

import scala.{xml => X}
import sjc.delta.Delta.DeltaOps
import sjc.delta.matchers.{beDifferentTo, beIdenticalTo}
import sjc.delta.matchers.syntax._

import scala.reflect.ClassTag


class FpmlExample extends FreeSpec with Matchers {
  import FpmlExample._

  val pretty = new X.PrettyPrinter(100, 2)

  "blah" in {

    val decoded = DecodeNode.decode[FpML](xml)
    val original = removeNamespaces(xml)

    val Ok(fpml) = decoded

    val roundTripped = EncodeNode.encode(fpml)

//    println(pretty.formatNodes(roundTripped))

//    println("\n\n\n")


//    println(pretty.formatNodes(noNS))

//    println("\n\n\n")

    val originalRoundtripped = sjc.delta.std.xml("original", "roundTripped")
    import originalRoundtripped._

//    print(pretty.formatNodes(roundTripped))
//
    val delta = originalRoundtripped.nodeDelta.apply(original, roundTripped).asXml

    roundTripped should beIdenticalTo(original)

//    println(pretty.formatNodes(delta))
  }

  private def removeNamespaces(input: X.Node): X.Node = input match {
    case e: X.Elem => e.copy(scope = X.TopScope, attributes = removeNamespaces(e.attributes), child = e.child.map(removeNamespaces))
    case t: X.Text => t
    case other => other
  }

  private def removeNamespaces(meta: X.MetaData): X.MetaData = meta.filter {
    case X.PrefixedAttribute("xmlns", _, _, _) => false
    case other => true
  }
}

case class FpML(
  @element clearingConfirmed: ClearingConfirmed,
  @namespace("xmlns") xmlns: Namespace,
  @namespace("xmlns:cme") cme: Namespace,
  @namespace("xmlns:dsig") dsig: Namespace
)

object FpML {
  implicit val codec: CodecXml[FpML] = CodecXml.apply(FpML.apply _,  FpML.unapply _)(
    _.element("clearingConfirmed"), _.namespace("xmlns"), _.namespace("xmlns:cme"), _.namespace("xmlns:dsig")
  )
}

case class ClearingConfirmed(
  @attribute fpmlVersion: String,
  @element header: Header,
  @element trade: Trade,
  @element parties: List[Party],
  @element account: Account
)

object ClearingConfirmed {
  implicit val codec: CodecXml[ClearingConfirmed] = CodecXml(ClearingConfirmed.apply _, ClearingConfirmed.unapply _)(
    _.attribute("fpmlVersion"), _.element("header"), _.element("trade"), _.element("party"), _.element("account")
  )
}

case class Account(
  @attribute id: String,
  @element accountId: AccountId,
  @element servicingParty: Reference
)

object Account {
  implicit val codec: CodecXml[Account] = CodecXml(Account.apply _, Account.unapply _)(
    _.attribute("id"), _.element("accountId"), _.element("servicingParty")
  )
}

case class AccountId(
  @attribute("accountIdScheme") scheme: String,
  @text value: String
)

object AccountId {
  implicit val codec: CodecXml[AccountId] = CodecXml(AccountId.apply _, AccountId.unapply _)(
    _.attribute("accountIdScheme"), _.text
  )
}

case class Party(
  @attribute id: String,
  @attribute partyId: PartyId
)

object Party {
  implicit val codec: CodecXml[Party] = CodecXml(Party.apply _, Party.unapply _)(
    _.attribute("id"), _.element("partyId")
  )
}

case class PartyId(
  @attribute("partyIdScheme") scheme: Option[String],
  @text value: String
)

object PartyId {
  implicit val codec: CodecXml[PartyId] = CodecXml(PartyId.apply _, PartyId.unapply _)(
    _.attribute("partyIdScheme"), _.text
  )
}

case class Header(
  @element messageId: MessageId,
  @element sentBy: SentBy,
  @element sentTo: List[SendTo],
  @element creationTimestamp: CreationTimestamp
)

object Header {
  implicit val codec: CodecXml[Header] = CodecXml(Header.apply _, Header.unapply _)(
    _.element("messageId"), _.element("sentBy"), _.element("sendTo"), _.element("creationTimestamp")
  )
}

case class MessageId(
  @attribute("messageIdScheme") scheme: String,
  @text value: String
)

object MessageId {
  implicit val codec: CodecXml[MessageId] = CodecXml(MessageId.apply _, MessageId.unapply _)(
    _.attribute("messageIdScheme"), _.text
  )
}

case class SentBy(
  @attribute("messageAddressScheme") scheme: String,
  @text value: String
)

object SentBy {
  implicit val codec: CodecXml[SentBy] = CodecXml(SentBy.apply _, SentBy.unapply _)(
    _.attribute("messageAddressScheme"), _.text
  )
}

case class SendTo(
  @attribute("messageAddressScheme") scheme: String,
  @text value: String
)

object SendTo {
  implicit val codec: CodecXml[SendTo] = CodecXml(SendTo.apply _, SendTo.unapply _)(
    _.attribute("messageAddressScheme"), _.text
  )
}

@text
case class CreationTimestamp(@text value: String)
object CreationTimestamp extends CodecXml.HasText[CreationTimestamp](new CreationTimestamp(_), _.value)

case class Trade(
  @element("tradeHeader") header: TradeHeader,
  @element swap: Swap
)

object Trade {
  implicit val codec: CodecXml[Trade] = CodecXml(Trade.apply _, Trade.unapply _)(
    _.element("tradeHeader"), _.element("swap")
  )
}

case class TradeHeader(
  @namespace("xmlns:xsi") xsiNamespace: Namespace,
  @attribute("xsi:type") headerType: String,
  @element partyTradeIdentifier: PartyTradeIdentifier,
  @element partyTradeInformation: PartyTradeInformation,
  @element tradeDate: FDate,
  @element clearedDate: FDate,
  @element("cme:originatingEvent") originatingEvent: OriginatingEvent,
  @element("cme:status") status: Status,
  @element("cme:universalSwapIdentifier") universalSwapIdentifier: UniversalSwapIdentifier,
  @element("cme:creditLimitInformation") creditLimitInformation: CreditLimitInformation
)

object TradeHeader {
  implicit val codec: CodecXml[TradeHeader] = CodecXml(TradeHeader.apply _, TradeHeader.unapply _)(
    _.namespace("xmlns:xsi"), _.attribute("xsi:type"),
    _.element("partyTradeIdentifier"), _.element("partyTradeInformation"), _.element("tradeDate"), _.element("clearedDate"),
    _.element("cme:originatingEvent"), _.element("cme:status"), _.element("cme:universalSwapIdentifier"),
    _.element("cme:creditLimitInformation")
  )
}

case class FDate(@text value: String)
object FDate extends CodecXml.HasText[FDate](new FDate(_), _.value)

case class OriginatingEvent(@text value: String)
object OriginatingEvent extends CodecXml.HasText[OriginatingEvent](new OriginatingEvent(_), _.value)

case class PartyTradeIdentifier(
  @element partyReference: Reference,
  @element tradeIds: List[TradeId]
)

object PartyTradeIdentifier {
  implicit val codec: CodecXml[PartyTradeIdentifier] = CodecXml(PartyTradeIdentifier.apply _, PartyTradeIdentifier.unapply _)(
    _.element("partyReference"), _.element("tradeId")
  )
}

case class Reference(href: String)
object Reference extends CodecXml.HasAttriubte[Reference](new Reference(_), _.href, "href")

case class TradeId(scheme: String, value: String)

object TradeId {
  implicit val codec: CodecXml[TradeId] = CodecXml(TradeId.apply _, TradeId.unapply _)(
    _.attribute("tradeIdScheme"), _.text
  )
}

case class PartyTradeInformation(
  partyReference: Reference, accountReference: Reference, relatedParty: RelatedParty, category: Category
)

object PartyTradeInformation {
  implicit val codec: CodecXml[PartyTradeInformation] = CodecXml(PartyTradeInformation.apply _, PartyTradeInformation.unapply _)(
    _.element("partyReference"), _.element("accountReference"), _.element("relatedParty"), _.element("category")
  )
}

case class RelatedParty(partyReference: Reference, role: Role)

object RelatedParty {
  implicit val codec: CodecXml[RelatedParty] = CodecXml(RelatedParty.apply _, RelatedParty.unapply _)(
    _.element("partyReference"), _.element("role")
  )
}

case class Role(value: String)
object Role extends CodecXml.HasText[Role](new Role(_), _.value)

case class Status(value: String)
object Status extends CodecXml.HasText[Status](new Status(_), _.value)

case class Category(scheme: String, value: String)

object Category {
  implicit val codec: CodecXml[Category] = CodecXml(Category.apply _, Category.unapply _)(
    _.attribute("categoryScheme"), _.text
  )
}

case class UniversalSwapIdentifier(issuer: Issuer, usi: Usi)

object UniversalSwapIdentifier {
  implicit val codec: CodecXml[UniversalSwapIdentifier] = CodecXml(UniversalSwapIdentifier.apply _, UniversalSwapIdentifier.unapply _)(
    _.element("cme:issuer"), _.element("cme:usi")
  )
}

case class CreditLimitInformation(limitApplicable: LimitApplicable)

object CreditLimitInformation {
  implicit val codec: CodecXml[CreditLimitInformation] = CodecXml(CreditLimitInformation.apply _, CreditLimitInformation.unapply _)(
    _.element("cme:limitApplicable")
  )
}

case class LimitApplicable(
  level: Level, limitType: LimitType, limitAmount: LimitAmount, amountUtilized: Amount, amountRemaining: Amount,
  limitImpactDueToTrade: Amount, currency: Currency
)

object LimitApplicable {
  implicit val codec: CodecXml[LimitApplicable] = CodecXml(LimitApplicable.apply _, LimitApplicable.unapply _)(
    _.element("cme:level"), _.element("cme:limitType"), _.element("cme:limitAmount"),
    _.element("cme:amountUtilized"), _.element("cme:amountRemaining"), _.element("cme:limitImpactDueToTrade"),
    _.element("cme:currency")
  )
}

case class Level(scheme: String, value: String)

object Level {
  implicit val codec: CodecXml[Level] = CodecXml(Level.apply _, Level.unapply _)(
    _.attribute("creditLimitLevelScheme"), _.text
  )
}

case class LimitType(scheme: String, value: String)

object LimitType {
  implicit val codec: CodecXml[LimitType] = CodecXml(LimitType.apply _, LimitType.unapply _)(
    _.attribute("creditLimitTypeScheme"), _.text
  )
}

case class LimitAmount(value: String)
object LimitAmount extends CodecXml.HasText[LimitAmount](new LimitAmount(_), _.value)

case class Amount(value: String)
object Amount extends CodecXml.HasText[Amount](new Amount(_), _.value)

case class Currency(scheme: Option[String], value: String)

object Currency {
  implicit val codec: CodecXml[Currency] = CodecXml(Currency.apply _, Currency.unapply _)(
    _.attribute("currencyScheme"), _.text
  )
}

case class Issuer(scheme: String, value: String)

object Issuer {
  implicit val codec: CodecXml[Issuer] = CodecXml(Issuer.apply _, Issuer.unapply _)(
    _.attribute("issuerIdScheme"), _.text
  )
}

case class Usi(scheme: String, value: String)

object Usi {
  implicit val codec: CodecXml[Usi] = CodecXml(Usi.apply _, Usi.unapply _)(
    _.attribute("usiScheme"), _.text
  )
}


case class Swap(swapStream: List[SwapStream], additionalPayment: AdditionalPayment)

object Swap {
  implicit val codec: CodecXml[Swap] = CodecXml(Swap.apply _, Swap.unapply _)(
    _.element("swapStream"), _.element("additionalPayment")
  )
}

case class AdditionalPayment(
  payerPartyReference: Reference, receiverPartyReference: Reference, paymentAmount: PaymentAmount,
  paymentDate: PaymentDate, paymentType: String
)

object AdditionalPayment {
  implicit val codec: CodecXml[AdditionalPayment] = CodecXml(AdditionalPayment.apply _, AdditionalPayment.unapply _)(
    _.element("payerPartyReference"), _.element("receiverPartyReference"), _.element("paymentAmount"), _.element("paymentDate"),
    _.element("paymentType")
  )
}

case class PaymentDate(unadjustedDate: FDate, dateAdjustments: DateAdjustments)

object PaymentDate {
  implicit val codec: CodecXml[PaymentDate] = CodecXml(PaymentDate.apply _, PaymentDate.unapply _)(
    _.element("unadjustedDate"), _.element("dateAdjustments")
  )
}

case class PaymentAmount(currency: Currency, amount: String)

object PaymentAmount {
  implicit val codec: CodecXml[PaymentAmount] = CodecXml(PaymentAmount.apply _, PaymentAmount.unapply _)(
    _.element("currency"), _.element("amount")
  )
}

case class SwapStream(
  id: String, payerPartyReference: Reference, payerAccountReference: Option[Reference],
  receiverPartyReference: Reference,
  receiverAccountReference: Option[Reference],
  calculationPeriodDates: CalculationPeriodDates, paymentDates: PaymentDates, resetDates: Option[ResetDates],
  calculationPeriodAmount: CalculationPeriodAmount, cashflows: Cashflows
)

object SwapStream {
  implicit val codec: CodecXml[SwapStream] = CodecXml(SwapStream.apply _, SwapStream.unapply _)(
    _.attribute("id"), _.element("payerPartyReference"), _.element("payerAccountReference"),
    _.element("receiverPartyReference"), _.element("receiverAccountReference"),
    _.element("calculationPeriodDates"), _.element("paymentDates"), _.element("resetDates"), _.element("calculationPeriodAmount"),
    _.element("cashflows")
  )
}

case class Cashflows(cashflowsMatchParameters: Boolean, paymentCalculationPeriod: List[PaymentCalculationPeriod])

object Cashflows {
  implicit val codce: CodecXml[Cashflows] = CodecXml(Cashflows.apply _, Cashflows.unapply _)(
    _.element("cashflowsMatchParameters"), _.element("paymentCalculationPeriod")
  )
}

case class PaymentCalculationPeriod(adjustedPaymentDate: FDate, calculationPeriod: CalculationPeriod)

object PaymentCalculationPeriod {
  implicit val codec: CodecXml[PaymentCalculationPeriod] = CodecXml(PaymentCalculationPeriod.apply _, PaymentCalculationPeriod.unapply _)(
    _.element("adjustedPaymentDate"), _.element("calculationPeriod")
  )
}

case class CalculationPeriod(
  adjustedStartDate: FDate, adjustedEndDate: FDate, notionalAmount: Amount,
  floatingRateDefinition: Option[FloatingRateDefinition], fixedRate: Option[String]
)

object CalculationPeriod {
  implicit val codec: CodecXml[CalculationPeriod] = CodecXml(CalculationPeriod.apply _, CalculationPeriod.unapply _)(
    _.element("adjustedStartDate"), _.element("adjustedEndDate"), _.element("notionalAmount"),
    _.element("floatingRateDefinition"), _.element("fixedRate")
  )
}

case class FloatingRateDefinition(rateObservation: RateObservation)

object FloatingRateDefinition {
  implicit val codec: CodecXml[FloatingRateDefinition] = CodecXml(FloatingRateDefinition.apply _, FloatingRateDefinition.unapply _)(
    _.element("rateObservation")
  )
}

case class RateObservation(adjustedFixingDate: FDate, observedRate: String)

object RateObservation {
  implicit val codce: CodecXml[RateObservation] = CodecXml(RateObservation.apply _, RateObservation.unapply _)(
    _.element("adjustedFixingDate"), _.element("observedRate")
  )
}

case class CalculationPeriodAmount(calculation: Calculation)

object CalculationPeriodAmount {
  implicit val codec: CodecXml[CalculationPeriodAmount] = CodecXml(CalculationPeriodAmount.apply _, CalculationPeriodAmount.unapply _)(
    _.element("calculation")
  )
}

case class Calculation(
  notionalSchedule: NotionalSchedule, fixedRateSchedule: Option[FixedRateSchedule],
  floatingRateCalculation: Option[FloatingRateCalculation], dayCountFraction: String
)

object Calculation {
  implicit val codec: CodecXml[Calculation] = CodecXml(Calculation.apply _, Calculation.unapply _)(
    _.element("notionalSchedule"), _.element("fixedRateSchedule"),
    _.element("floatingRateCalculation"), _.element("dayCountFraction")
  )
}

case class FixedRateSchedule(initialValue: String)

object FixedRateSchedule {
  implicit val codec: CodecXml[FixedRateSchedule] = CodecXml(FixedRateSchedule.apply _, FixedRateSchedule.unapply _)(
    _.element("initialValue")
  )
}

case class FloatingRateCalculation(floatingRateIndex: String, indexTenor: Frequency)

object FloatingRateCalculation {
  implicit val codec: CodecXml[FloatingRateCalculation] = CodecXml(FloatingRateCalculation.apply _, FloatingRateCalculation.unapply _)(
    _.element("floatingRateIndex"), _.element("indexTenor")
  )
}

case class NotionalSchedule(notionalStepSchedule: NotionalStepSchedule)

object NotionalSchedule {
  implicit val codcc: CodecXml[NotionalSchedule] = CodecXml(NotionalSchedule.apply _, NotionalSchedule.unapply _)(
    _.element("notionalStepSchedule")
  )
}

case class NotionalStepSchedule(initialValue: String, currency: Currency)

object NotionalStepSchedule {
  implicit val codcc: CodecXml[NotionalStepSchedule] = CodecXml(NotionalStepSchedule.apply _, NotionalStepSchedule.unapply _)(
    _.element("initialValue"), _.element("currency")
  )
}

case class ResetDates(
  id: String, calculationPeriodDatesReference: Reference, resetRelativeTo: String,
  fixingDates: FixingDates, resetFrequency: Frequency, resetDatesAdjustments: DateAdjustments
)

object ResetDates {
  implicit val codec: CodecXml[ResetDates] = CodecXml(ResetDates.apply _, ResetDates.unapply _)(
    _.attribute("id"), _.element("calculationPeriodDatesReference"), _.element("resetRelativeTo"),
    _.element("fixingDates"), _.element("resetFrequency"), _.element("resetDatesAdjustments")
  )
}

case class FixingDates(
  periodMultiplier: PeriodMultiplier, period: Period, businessDayConvention: BusinessDayConvention,
  businessCenters: BusinessCenters, dateRelativeTo: Reference
)

object FixingDates {
  implicit val codec: CodecXml[FixingDates] = CodecXml(FixingDates.apply _, FixingDates.unapply _)(
    _.element("periodMultiplier"), _.element("period"), _.element("businessDayConvention"),
    _.element("businessCenters"), _.element("dateRelativeTo")
  )
}

case class PaymentDates(
  calculationPeriodDatesReference: Reference, paymentFrequency: Frequency, payRelativeTo: String,
  paymentDatesAdjustments: DateAdjustments
)

object PaymentDates {
  implicit val codec: CodecXml[PaymentDates] = CodecXml(PaymentDates.apply _, PaymentDates.unapply _)(
    _.element("calculationPeriodDatesReference"), _.element("paymentFrequency"), _.element("payRelativeTo"),
    _.element("paymentDatesAdjustments")
  )
}

case class CalculationPeriodDates(
  id: String, effectiveDate: AdjustableDate, terminationDate: AdjustableDate,
  calculationPeriodDatesAdjustments: DateAdjustments, calculationPeriodFrequency: Frequency
)

object CalculationPeriodDates {
  implicit val codec: CodecXml[CalculationPeriodDates] = CodecXml(CalculationPeriodDates.apply _, CalculationPeriodDates.unapply _)(
    _.attribute("id"), _.element("effectiveDate"), _.element("terminationDate"), _.element("calculationPeriodDatesAdjustments"),
    _.element("calculationPeriodFrequency")
  )
}

case class AdjustableDate(unadjustedDate: FDate, dateAdjustments: DateAdjustments)

object AdjustableDate {
  implicit val codec: CodecXml[AdjustableDate] = CodecXml(AdjustableDate.apply _, AdjustableDate.unapply _)(
    _.element("unadjustedDate"), _.element("dateAdjustments")
  )
}

case class DateAdjustments(
  businessDayConvention: BusinessDayConvention, businessCentersReference: Option[Reference],
  businessCenters: Option[BusinessCenters]
)

object DateAdjustments {
  implicit val codec: CodecXml[DateAdjustments] = CodecXml(DateAdjustments.apply _, DateAdjustments.unapply _)(
    _.element("businessDayConvention"), _.element("businessCentersReference"), _.element("businessCenters")
  )
}

case class BusinessCenters(id: Option[String], value: String)

object BusinessCenters {
  implicit val codec: CodecXml[BusinessCenters] = CodecXml(BusinessCenters.apply _, BusinessCenters.unapply _)(
    _.attribute("id"), _.element("businessCenter")
  )
}

case class Frequency(periodMultiplier: PeriodMultiplier, period: Period, rollConvention: Option[String])

object Frequency {
  implicit val codec: CodecXml[Frequency] = CodecXml(Frequency.apply _, Frequency.unapply _)(
    _.element("periodMultiplier"), _.element("period"), _.element("rollConvention")
  )
}

case class Period(value: String)
object Period extends CodecXml.HasText[Period](new Period(_), _.value)

case class PeriodMultiplier(value: String)
object PeriodMultiplier extends CodecXml.HasText[PeriodMultiplier](new PeriodMultiplier(_), _.value)

case class BusinessDayConvention(value: String)
object BusinessDayConvention extends CodecXml.HasText[BusinessDayConvention](new BusinessDayConvention(_), _.value)

case class Namespace(value: String)

object Namespace {
  implicit val codec: CodecAttribute[Namespace] = CodecAttribute.of[String].xmap[Namespace](Namespace(_))(_.value)
}

object FpmlExample {
  val xml: X.Node =
  <cme:FpML xmlns="http://www.fpml.org/FpML-5/confirmation" xmlns:cme="http://www.cmegroup.com/otc-clearing/confirmation" xmlns:dsig="http://www.w3.org/2000/09/xmldsig#">
  	<clearingConfirmed fpmlVersion="5-0">
  		<header>
  			<messageId messageIdScheme="cme_message_id">1242553860</messageId>
  			<sentBy messageAddressScheme="cme_clearingorg_id">CME</sentBy>
  			<sendTo messageAddressScheme="cme_firm_id">Firm</sendTo>
  			<sendTo messageAddressScheme="cme_exchange_id">CME</sendTo>
  			<creationTimestamp>2014-10-03T07:42:36-05:00</creationTimestamp>
  		</header>

  		<trade>
  			<tradeHeader xsi:type="cme:CMETradeHeader" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
  				<partyTradeIdentifier>
  					<partyReference href="clearing_service"/>
  					<tradeId tradeIdScheme="cme_trade_id">2553860</tradeId>
  					<tradeId tradeIdScheme="client_trade_id">542E99BB1464001F</tradeId>
  					<tradeId tradeIdScheme="platform_trade_id">FIT:20141003:3739:25:50019:1</tradeId>
  					<tradeId tradeIdScheme="block_trade_id">FIT:20141003:3739:25:50019</tradeId>
  				</partyTradeIdentifier>

  				<partyTradeInformation>
  					<partyReference href="clearing_firm"/>
  					<accountReference href="account1"/>
  					<relatedParty>
  						<partyReference href="trade_source"/>
  						<role>InputSource</role>
  					</relatedParty>
  					<category categoryScheme="cme_origin_code">HOUS</category>
  				</partyTradeInformation>

  				<tradeDate>2014-10-03</tradeDate>
  				<clearedDate>2014-10-03</clearedDate>
  				<cme:originatingEvent>NEW_TRADE</cme:originatingEvent>
  				<cme:status>CLEARED</cme:status>
  				<cme:universalSwapIdentifier>
  					<cme:issuer issuerIdScheme="cftc_Namespace">1010000023</cme:issuer>
  					<cme:usi usiScheme="reg_trade_id">CCCIRS25</cme:usi>
  				</cme:universalSwapIdentifier>
  				<cme:creditLimitInformation>
  					<cme:limitApplicable>
  						<cme:level creditLimitLevelScheme="creditLimitLevelScheme">HOUS</cme:level>
  						<cme:limitType creditLimitTypeScheme="http://www.fpml.org/coding-scheme/credit-limit-type">PV01</cme:limitType>
  						<cme:limitAmount>999999999999998980</cme:limitAmount>
  						<cme:amountUtilized>-12088</cme:amountUtilized>
  						<cme:amountRemaining>999999999999986940</cme:amountRemaining>
  						<cme:limitImpactDueToTrade>-30035</cme:limitImpactDueToTrade>
  						<cme:currency currencyScheme="http://www.fpml.org/ext/iso4217-2001-08-15">USD</cme:currency>
  					</cme:limitApplicable>
  				</cme:creditLimitInformation>
  			</tradeHeader>

  			<swap>
  				<swapStream id="floatLeg">
  					<payerPartyReference href="clearing_firm"/>
  					<payerAccountReference href="account1"/>
  					<receiverPartyReference href="clearing_service"/>

  					<calculationPeriodDates id="floatingCalcPeriodDates">
  						<effectiveDate>
  							<unadjustedDate>2014-10-07</unadjustedDate>
  							<dateAdjustments>
  								<businessDayConvention>NONE</businessDayConvention>
  							</dateAdjustments>
  						</effectiveDate>

  						<terminationDate>
  							<unadjustedDate>2015-10-07</unadjustedDate>
  							<dateAdjustments>
  								<businessDayConvention>MODFOLLOWING</businessDayConvention>
  								<businessCentersReference href="floatPrimaryBusinessCenters"/>
  							</dateAdjustments>
  						</terminationDate>

  						<calculationPeriodDatesAdjustments>
  							<businessDayConvention>MODFOLLOWING</businessDayConvention>
  							<businessCentersReference href="floatPrimaryBusinessCenters"/>
  						</calculationPeriodDatesAdjustments>

  						<calculationPeriodFrequency>
  							<periodMultiplier>6</periodMultiplier>
  							<period>M</period>
  							<rollConvention>7</rollConvention>
  						</calculationPeriodFrequency>

  					</calculationPeriodDates>

  					<paymentDates>
  						<calculationPeriodDatesReference href="floatingCalcPeriodDates"/>
  						<paymentFrequency>
  							<periodMultiplier>6</periodMultiplier>
  							<period>M</period>
  						</paymentFrequency>
  						<payRelativeTo>CalculationPeriodEndDate</payRelativeTo>
  						<paymentDatesAdjustments>
  							<businessDayConvention>MODFOLLOWING</businessDayConvention>
  							<businessCenters id="floatPrimaryBusinessCenters">
  								<businessCenter>AUSY</businessCenter>
  							</businessCenters>
  						</paymentDatesAdjustments>
  					</paymentDates>
  					<resetDates id="floatingLegResetDates">
  						<calculationPeriodDatesReference href="floatingCalcPeriodDates"/>
  						<resetRelativeTo>CalculationPeriodStartDate</resetRelativeTo>
  						<fixingDates>
  							<periodMultiplier>0</periodMultiplier>
  							<period>D</period>
  							<businessDayConvention>PRECEDING</businessDayConvention>
  							<businessCenters>
  								<businessCenter>AUSY</businessCenter>
  							</businessCenters>
  							<dateRelativeTo href="floatingLegResetDates"/>
  						</fixingDates>
  						<resetFrequency>
  							<periodMultiplier>6</periodMultiplier>
  							<period>M</period>
  						</resetFrequency>
  						<resetDatesAdjustments>
  							<businessDayConvention>MODFOLLOWING</businessDayConvention>
  							<businessCentersReference href="floatPrimaryBusinessCenters"/>
  						</resetDatesAdjustments>
  					</resetDates>
  					<calculationPeriodAmount>
  						<calculation>
  							<notionalSchedule>
  								<notionalStepSchedule>
  									<initialValue>345000000.00</initialValue>
  									<currency>AUD</currency>
  								</notionalStepSchedule>
  							</notionalSchedule>
  							<floatingRateCalculation>
  								<floatingRateIndex>AUD-BBR-BBSW</floatingRateIndex>
  								<indexTenor>
  									<periodMultiplier>6</periodMultiplier>
  									<period>M</period>
  								</indexTenor>
  							</floatingRateCalculation>
  							<dayCountFraction>ACT/365.FIXED</dayCountFraction>
  						</calculation>
  					</calculationPeriodAmount>

  					<cashflows>
  						<cashflowsMatchParameters>true</cashflowsMatchParameters>
  						<paymentCalculationPeriod>
  							<adjustedPaymentDate>2015-04-07</adjustedPaymentDate>
  							<calculationPeriod>
  								<adjustedStartDate>2014-10-07</adjustedStartDate>
  								<adjustedEndDate>2015-04-07</adjustedEndDate>
  								<notionalAmount>345000000.00</notionalAmount>
  								<floatingRateDefinition>
  									<rateObservation>
  										<adjustedFixingDate>2014-10-07</adjustedFixingDate>
  										<observedRate>0</observedRate>
  									</rateObservation>
  								</floatingRateDefinition>
  							</calculationPeriod>
  						</paymentCalculationPeriod>
  						<paymentCalculationPeriod>
  							<adjustedPaymentDate>2015-10-07</adjustedPaymentDate>
  							<calculationPeriod>
  								<adjustedStartDate>2015-04-07</adjustedStartDate>
  								<adjustedEndDate>2015-10-07</adjustedEndDate>
  								<notionalAmount>345000000.00</notionalAmount>
  								<floatingRateDefinition>
  									<rateObservation>
  										<adjustedFixingDate>2015-04-07</adjustedFixingDate>
  										<observedRate>0</observedRate>
  									</rateObservation>
  								</floatingRateDefinition>
  							</calculationPeriod>
  						</paymentCalculationPeriod>
  					</cashflows>
  				</swapStream>
  				<swapStream id="fixedLeg">
  					<payerPartyReference href="clearing_service"/>
  					<receiverPartyReference href="clearing_firm"/>
  					<receiverAccountReference href="account1"/>
  					<calculationPeriodDates id="fixedCalcPeriodDates">
  						<effectiveDate>
  							<unadjustedDate>2014-10-07</unadjustedDate>
  							<dateAdjustments>
  								<businessDayConvention>NONE</businessDayConvention>
  							</dateAdjustments>
  						</effectiveDate>
  						<terminationDate>
  							<unadjustedDate>2015-10-07</unadjustedDate>
  							<dateAdjustments>
  								<businessDayConvention>MODFOLLOWING</businessDayConvention>
  								<businessCentersReference href="fixedPrimaryBusinessCenters"/>
  							</dateAdjustments>
  						</terminationDate>
  						<calculationPeriodDatesAdjustments>
  							<businessDayConvention>MODFOLLOWING</businessDayConvention>
  							<businessCentersReference href="fixedPrimaryBusinessCenters"/>
  						</calculationPeriodDatesAdjustments>
  						<calculationPeriodFrequency>
  							<periodMultiplier>6</periodMultiplier>
  							<period>M</period>
  							<rollConvention>7</rollConvention>
  						</calculationPeriodFrequency>
  					</calculationPeriodDates>
  					<paymentDates>
  						<calculationPeriodDatesReference href="fixedCalcPeriodDates"/>
  						<paymentFrequency>
  							<periodMultiplier>6</periodMultiplier>
  							<period>M</period>
  						</paymentFrequency>
  						<payRelativeTo>CalculationPeriodEndDate</payRelativeTo>
  						<paymentDatesAdjustments>
  							<businessDayConvention>MODFOLLOWING</businessDayConvention>
  							<businessCenters id="fixedPrimaryBusinessCenters">
  								<businessCenter>AUSY</businessCenter>
  							</businessCenters>
  						</paymentDatesAdjustments>
  					</paymentDates>
  					<calculationPeriodAmount>
  						<calculation>
  							<notionalSchedule>
  								<notionalStepSchedule>
  									<initialValue>345000000.00</initialValue>
  									<currency>AUD</currency>
  								</notionalStepSchedule>
  							</notionalSchedule>
  							<fixedRateSchedule>
  								<initialValue>0.03852</initialValue>
  							</fixedRateSchedule>
  							<dayCountFraction>ACT/365.FIXED</dayCountFraction>
  						</calculation>
  					</calculationPeriodAmount>
  					<cashflows>
  						<cashflowsMatchParameters>true</cashflowsMatchParameters>
  						<paymentCalculationPeriod>
  							<adjustedPaymentDate>2015-04-07</adjustedPaymentDate>
  							<calculationPeriod>
  								<adjustedStartDate>2014-10-07</adjustedStartDate>
  								<adjustedEndDate>2015-04-07</adjustedEndDate>
  								<notionalAmount>345000000.00</notionalAmount>
  								<fixedRate>0.03852</fixedRate>
  							</calculationPeriod>
  						</paymentCalculationPeriod>
  						<paymentCalculationPeriod>
  							<adjustedPaymentDate>2015-10-07</adjustedPaymentDate>
  							<calculationPeriod>
  								<adjustedStartDate>2015-04-07</adjustedStartDate>
  								<adjustedEndDate>2015-10-07</adjustedEndDate>
  								<notionalAmount>345000000.00</notionalAmount>
  								<fixedRate>0.03852</fixedRate>
  							</calculationPeriod>
  						</paymentCalculationPeriod>
  					</cashflows>
  				</swapStream>
  				<additionalPayment>
  					<payerPartyReference href="clearing_service"/>
  					<receiverPartyReference href="clearing_firm"/>
  					<paymentAmount>
  						<currency>AUD</currency>
  						<amount>9586.00</amount>
  					</paymentAmount>
  					<paymentDate>
  						<unadjustedDate>2014-10-08</unadjustedDate>
  						<dateAdjustments>
  							<businessDayConvention>MODFOLLOWING</businessDayConvention>
  							<businessCenters>
  								<businessCenter>AUSY</businessCenter>
  							</businessCenters>
  						</dateAdjustments>
  					</paymentDate>
  					<paymentType>UPFRONT_FEE</paymentType>
  				</additionalPayment>
  			</swap>
  		</trade>
  		<party id="clearing_firm">
  			<partyId partyIdScheme="clearing_member_firms">Firm</partyId>
  		</party>
  		<party id="clearing_service">
  			<partyId>CME</partyId>
  		</party>
  		<party id="trade_source">
  			<partyId>PartyID</partyId>
  		</party>
  		<account id="account1">
  			<accountId accountIdScheme="clearing_firm_accounts">Account-Receiving</accountId>
  			<servicingParty href="clearing_firm"/>
  		</account>
  	</clearingConfirmed>
  </cme:FpML>
}

case class text() extends scala.annotation.StaticAnnotation
case class attribute(name: String = null) extends scala.annotation.StaticAnnotation
case class element(name: String = null) extends scala.annotation.StaticAnnotation
case class namespace(name: String = null) extends scala.annotation.StaticAnnotation

object namespace {
  def apply(name: String): namespace = new namespace(Some(name))
}

